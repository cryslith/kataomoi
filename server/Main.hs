import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import qualified Network.Socket as S
import qualified Text.JSON as J
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Control.Exception as C
import qualified Data.Foldable as F (mapM_)
import Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions, pendingStream)
import Control.Monad (forever, when, void)
import Control.Concurrent (MVar, newMVar, modifyMVarMasked_, readMVar, swapMVar, forkIO, forkIOWithUnmask)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Data.IORef
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

type User = String
type PublicKey = String
type UserData = (Maybe (TChan String), PublicKey)

type Room = String
type RoomData = Map.Map User UserData

-- Functions to create, update, or delete a room and indicate success
-- or failure. In the case of failure, the room must remain unchanged.
type RoomAlterer = Maybe RoomData -> (Bool, Maybe RoomData)

type ServerState = Map.Map Room RoomData

port = 9001

newServerState :: ServerState
newServerState = Map.empty

data LogLevel =
      None     -- don't print anything
    | Error    -- print details of errors
    | Event    -- print high-level descriptions of all events
    | Message  -- print all non-keepalive messages, incoming or outgoing
    | All      -- print all messages (there will be lots of keepalive spew)
    deriving (Eq, Ord, Show, Read)

fmtSubject :: Maybe Room -> Maybe User -> String
fmtSubject r n =
    case (r, n) of
      (Just room, Just name) ->
          name ++ "@" ++ room
      (Just room, Nothing) -> "#" ++ room
      _ -> "[anon]"

logEntry :: MVar LogLevel -> LogLevel -> Maybe Room -> Maybe User -> String -> IO ()
logEntry mlevel entryLevel r n desc = do
  curLevel <- readMVar mlevel
  when (curLevel >= entryLevel) $ do
    time <- getZonedTime
    let fmtTime = formatTime defaultTimeLocale "%F %T %z" time
    putStrLn $ fmtTime ++ " " ++ fmtSubject r n ++ ": " ++ show entryLevel ++ ": " ++ desc


usersJSON :: RoomData -> J.JSObject J.JSValue
usersJSON =
    J.toJSObject .
    Map.toList .
    Map.map (\(mchan, key) ->
                 J.JSObject $
                 J.toJSObject
                      [ ( "pubkey"
                        , J.showJSON key)
                      , ( "connected"
                        , J.showJSON $ isJust mchan)
                      ]
            )

at :: (Functor f, Ord k) => k -> (Maybe a -> f (Maybe a)) -> Map.Map k a -> f (Map.Map k a)
at k f m = let mv = Map.lookup k m
               fmv' = f mv
           in fmap (\mv' -> case mv' of
                              Just v' -> Map.insert k v' m
                              Nothing -> Map.delete k m
                   )
                   fmv'

alterRoom :: MVar ServerState -> Room -> RoomAlterer -> (Bool -> IO ()) -> IO ()
alterRoom mstate room f react =
    do
      modifyMVarMasked_ mstate (at room $ reactAndSend . f)
    where reactAndSend (b, mrd) = do
            react b
            when b $ F.mapM_ delegateSendAllUsers mrd
            return mrd

-- If the requested name is available, adds a user to a room, creating
-- the room if necessary. Returns whether the user was successfully
-- added, and Just the room, updated to contain the new user if the
-- name was available.
addUser :: User -> UserData -> RoomAlterer
addUser u ud mrd =
    let rd = fromMaybe Map.empty mrd
    in
      if Map.member u rd
      then (False, mrd)
      else (True, Just $ Map.insert u ud rd)

-- If the room exists and contains the user, modifies the room to mark
-- the user as disconnected. Always returns True for success status;
-- if all users in the room are now disconnected, returns Nothing for
-- result, otherwise returns Just the room.
markUserDisconnected :: User -> RoomAlterer
markUserDisconnected u mrd =
    (True,
     case mrd of
       Nothing -> Nothing
       Just rd ->
           let rd' = Map.adjust (\(_, k) -> (Nothing, k)) u rd
           in
             if Map.null $ Map.filter (\(mc, _) -> isJust mc) rd'
             then Nothing
             else Just rd'
    )

send :: WS.Connection -> (String -> IO ()) -> String -> IO ()
send conn logger s = do
  WS.sendTextData conn $ T.pack s
  logger s

sendMsg :: WS.Connection -> (String -> IO ()) -> String -> [(String, J.JSValue)] -> IO ()
sendMsg conn logger t = (send conn logger) .
                        J.encode .
                        J.toJSObject .
                        (("type", J.showJSON t):)

delegateSend :: TChan String -> String -> IO ()
delegateSend c s = atomically $ writeTChan c s

delegateSendUsers :: TChan String -> RoomData -> IO ()
delegateSendUsers chan rd = delegateSend chan $ J.encode $
  J.toJSObject [ ("type", J.showJSON "users")
               , ("users", J.JSObject $ usersJSON rd)
               ]

delegateSendAllUsers :: RoomData -> IO ()
delegateSendAllUsers rd =
    F.mapM_ (\(mchan, _) ->
               F.mapM_ (flip delegateSendUsers rd) mchan
            )
            rd

data ForwardError = Nonexistent | Disconnected

forwardMessage :: ServerState -> Room -> User -> String -> IO (Maybe ForwardError)
forwardMessage state room user msg =
  case Map.lookup room state >>= Map.lookup user of
    Just (Just chan, _) -> do
      delegateSend chan msg
      return Nothing
    Just (Nothing, _) -> return $ Just Disconnected
    Nothing -> return $ Just Nonexistent

sendWelcome :: WS.Connection -> (String -> IO ()) -> Room -> User -> IO ()
sendWelcome conn logger r u =
    sendMsg conn logger "welcome"
                [ ("room", J.showJSON r)
                , ("name", J.showJSON u)
                ]

sendUnavailable :: WS.Connection -> (String -> IO ()) -> Room -> User -> IO ()
sendUnavailable conn logger r u =
    sendMsg conn logger "unavailable"
                [ ("room", J.showJSON r)
                , ("name", J.showJSON u)
                ]

sendDisconnected :: WS.Connection -> (String -> IO ()) -> User -> IO ()
sendDisconnected conn logger recipient =
    sendMsg conn logger "disconnected"
                [("recipient", J.showJSON recipient)]

sendError :: WS.Connection -> (String -> IO ()) -> String -> String -> IO ()
sendError conn logger s m =
    sendMsg conn logger "error"
                 [ ("error", J.showJSON s)
                 , ("message", J.showJSON m)
                 ]

sendKeepalive :: WS.Connection -> (String -> IO ()) -> IO ()
sendKeepalive conn logger =
    sendMsg conn logger "keepalive" []


catchConnEx :: IO a -> (WS.ConnectionException -> IO a) -> IO a
catchConnEx = C.catch

dieOnConnEx :: IO () -> IO ()
dieOnConnEx = flip catchConnEx $ const $ return ()

server :: MVar ServerState -> MVar LogLevel -> WS.ServerApp
server mstate mlevel pending = do
  conn <- WS.acceptRequest pending
  chan <- atomically $ newTChan
  roomRef <- newIORef Nothing
  nameRef <- newIORef Nothing
  let logger lvl s = do
        r <- readIORef roomRef
        n <- readIORef nameRef
        logEntry mlevel lvl r n s
      sendLogger = (logger Message) . ("we sent " ++)
      sendErrLogger = (logger Error) . ("(sent to client) " ++)
  logger Event "connected"
  forkIO $ dieOnConnEx $ forever $ do
    s <- atomically $ readTChan chan
    r <- readIORef roomRef
    n <- readIORef nameRef
    send conn sendLogger s
  flip C.finally (disconnect conn roomRef nameRef) $ forever $ do
    text <- WS.receiveData conn
    let jsonString = T.unpack text
        y = do
          jsonData <- case J.decode jsonString of
                        J.Error e -> Nothing
                        J.Ok jsonData -> Just $ Map.fromList $
                                                  J.fromJSObject jsonData
          t <- Map.lookup "type" jsonData
          if t == "join" then
            do
              name <- Map.lookup "name" jsonData
              room <- Map.lookup "room" jsonData
              pubkey <- Map.lookup "pubkey" jsonData
              Just $ do
                n <- readIORef nameRef
                logger Message $ "sent us " ++ jsonString
                logger Event $ "attempted to join room " ++ room ++ " with name " ++ name
                if isJust n
                then sendError conn sendErrLogger "already joined" jsonString
                else
                    if usernameOK name && roomOK room
                    then do
                      alterRoom mstate room (addUser name (Just chan, pubkey))
                         (\success ->
                              if success
                              then do
                                atomicWriteIORef roomRef $ Just room
                                atomicWriteIORef nameRef $ Just name
                                logger Event "signed in"
                                sendWelcome conn sendLogger room name
                              else do
                                logger Event "requested name unavailable"
                                sendUnavailable conn sendLogger room name
                         )
                    else sendError conn sendErrLogger "invalid username or room" jsonString
          else if t == "keepalive" then Just $
               do
                 logger All $ "sent us " ++ jsonString
                 sendKeepalive conn $ (logger All) . ("we sent " ++)
          else if t == "client" then
            do
              sender <- Map.lookup "sender" jsonData
              recipient <- Map.lookup "recipient" jsonData
              Just $ do
                logger Message $ "sent us " ++ jsonString
                room <- readIORef roomRef
                name <- readIORef nameRef
                case (room, name) of
                  (Just r, Just n) ->
                      if n /= sender
                      then sendError conn sendErrLogger "incorrect sender" jsonString
                      else do
                        s <- readMVar mstate
                        merr <- forwardMessage s r recipient jsonString
                        case merr of
                          Nothing -> logger Event $ sender ++ " sent message to " ++ recipient
                          Just Nonexistent -> sendError conn sendErrLogger "no such recipient" jsonString
                          Just Disconnected ->
                              do
                                logger Event $ sender ++ " failed to send message to disconnected user " ++ recipient
                                sendDisconnected conn sendLogger recipient
                  _ -> sendError conn sendErrLogger "not joined yet" jsonString
          else Nothing
    case y of
      Nothing -> sendError conn sendErrLogger "bad message" jsonString
      Just x -> x
  where
    disconnect conn roomRef nameRef = do
      -- We don't need to close the connection, because we should only
      -- get here if the client closed the connection already
      room <- readIORef roomRef
      name <- readIORef nameRef
      case (room, name) of
        (Just r, Just n) -> alterRoom mstate r (markUserDisconnected n) (const $ return ())
        _ -> return ()
      logEntry mlevel Event room name "disconnected"

usernameOK :: String -> Bool
usernameOK = alnum 8

roomOK :: String -> Bool
roomOK = alnum 20

alnum :: Int -> String -> Bool
alnum n s =
  length s >= 1 &&
  length s <= n &&
  all (\c -> 'a' <= c && c <= 'z' ||
             'A' <= c && c <= 'Z' ||
             '0' <= c && c <= '9') s

tryRead :: (Read a) => String -> Maybe a
tryRead = fmap fst . listToMaybe . reads

console :: MVar ServerState -> MVar LogLevel -> IO ()
console mstate mlevel =
    forever $ do
      inp <- getLine
      let inpWords = words inp
      case inpWords of
        ("help":_) ->
            putStrLn "Valid commands:\n  log (None|Error|Event|Message|All)\n  list [(_room_|*) [full]]"
        ("log":l:[]) ->
            case tryRead l of
              Just level ->
                  do
                    swapMVar mlevel level
                    putStrLn $ "log level set to " ++ l
              _ -> putStrLn $ "unknown log level: " ++ l
        ("list":args) ->
            do
              s <- readMVar mstate
              putStrLn "Rooms:"
              case args of
                [] -> mapM_ (putStrLn . ("  " ++)) (Map.keys s)
                (room:full) ->
                        let rooms = if room == "*"
                                    then Map.keys s
                                    else [room]
                            showUser u (mc, k) =
                                "    " ++
                                (if isJust mc
                                 then ""
                                 else "(x) ") ++
                                u ++
                                (if full == ["full"]
                                 then (" [" ++ k ++ "]")
                                 else "")
                        in mapM_ (\r ->
                                  case Map.lookup r s of
                                    Nothing -> putStrLn $ " " ++ r ++ ": empty"
                                    Just us -> do
                                      putStrLn $ "  " ++ r ++ ":"
                                      Map.foldrWithKey
                                             (\u ud m ->
                                                  m >> (putStrLn $ showUser u ud))
                                             (return ()) us
                                 ) rooms
        _ -> putStrLn $ "invalid command: " ++ inp


runLenientServer :: String -> Int -> WS.ServerApp -> IO ()
runLenientServer host port app = S.withSocketsDo $
  C.bracket
  (WS.makeListenSocket host port)
  S.close
  (\sock ->
    C.mask_ $ forever $ do
      C.allowInterrupt
      (conn, _) <- S.accept sock
      void $ forkIOWithUnmask $ \unmask ->
        dieOnConnEx $ C.finally (unmask $ runApp conn defaultConnectionOptions app) (S.close conn)
    )

runApp :: S.Socket -> ConnectionOptions -> WS.ServerApp -> IO ()
runApp socket opts app =
    C.bracket
        (WS.makePendingConnection socket opts)
        (Stream.close . pendingStream)
        app


main :: IO ()
main = do
  mstate <- newMVar newServerState
  mlevel <- newMVar Message
  hSetBuffering stdout LineBuffering
  putStrLn $ "Starting server on port " ++ show port ++ "..."
  forkIO $ console mstate mlevel
  runLenientServer "127.0.0.1" port $ server mstate mlevel
