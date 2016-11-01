import qualified Network.WebSockets as WS
import qualified Text.JSON as J
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Control.Exception as C
import qualified Data.Foldable as F (mapM_)
import Control.Monad (forever, when)
import Control.Concurrent (MVar, newMVar, modifyMVarMasked_, readMVar, forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Data.IORef
import Data.Maybe (isJust, fromMaybe)
import Control.Lens.At (at)

type User = String
type PublicKey = String
type UserData = (Maybe (TChan String), PublicKey)

type Room = String
type RoomData = Map.Map User UserData

-- Functions to create, update, or delete a room and indicate success
-- or failure. In the case of failure, the room must remain unchanged.
type RoomAlterer = Maybe RoomData -> (Bool, Maybe RoomData)

getConn :: UserData -> Maybe (TChan String)
getConn = fst

getKey :: UserData -> PublicKey
getKey = snd

type ServerState = Map.Map Room RoomData

port = 9001

newServerState :: ServerState
newServerState = Map.empty

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
           let rd' = Map.adjust (\(mc, k) -> (Nothing, k)) u rd
           in
             if Map.null $ Map.filter (\(mc, k) -> isJust mc) rd'
             then Nothing
             else Just rd'
    )

send :: WS.Connection -> String -> IO ()
send conn = WS.sendTextData conn . T.pack

sendJSON :: J.JSON a => WS.Connection -> a -> IO ()
sendJSON conn = (send conn) . J.encode

sendFromChan :: WS.Connection -> TChan String -> IO ()
sendFromChan conn chan = do
  s <- atomically $ readTChan chan
  send conn s

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

forwardMessage :: ServerState -> Room -> User -> String -> IO Bool
forwardMessage state room user msg =
  case Map.lookup room state >>= Map.lookup user of
    Just (Just chan, _) -> do
      delegateSend chan msg
      return True
    Nothing -> return False

sendWelcome :: WS.Connection -> Room -> User -> IO ()
sendWelcome conn r u =
    sendJSON conn $
         J.toJSObject [ ("type", J.showJSON "welcome")
                      , ("room", J.showJSON r)
                      , ("name", J.showJSON u)
                      ]

sendUnavailable :: WS.Connection -> Room -> User -> IO ()
sendUnavailable conn r u =
    sendJSON conn $
         J.toJSObject [ ("type", J.showJSON "unavailable")
                      , ("room", J.showJSON r)
                      , ("name", J.showJSON u)
                      ]

sendError :: WS.Connection -> String -> String -> IO ()
sendError conn s m =
    sendJSON conn $
         J.toJSObject [ ("type", J.showJSON "error")
                      , ("error", J.showJSON s)
                      , ("message", J.showJSON m)
                      ]

sendKeepalive :: WS.Connection -> IO ()
sendKeepalive conn =
  sendJSON conn $
       J.toJSObject [ ("type", J.showJSON "keepalive") ]

server :: MVar ServerState -> WS.ServerApp
server mstate pending = do
  conn <- WS.acceptRequest pending
  chan <- atomically $ newTChan
  roomRef <- newIORef Nothing
  nameRef <- newIORef Nothing
  forkIO $ forever $ sendFromChan conn chan
  flip C.finally (disconnect conn roomRef nameRef) $ forever $ do
    text <- WS.receiveData conn
    putStrLn (T.unpack text)
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
                if isJust n
                then sendError conn "already joined" jsonString
                else
                    if usernameOK name && roomOK room
                    then do
                      alterRoom mstate room (addUser name (Just chan, pubkey))
                         (\success ->
                              if success
                              then do
                                writeIORef roomRef $ Just room
                                writeIORef nameRef $ Just name
                                sendWelcome conn room name
                              else sendUnavailable conn room name
                         )
                    else sendError conn "invalid username or room" jsonString
          else if t == "keepalive" then Just $ sendKeepalive conn
          else if t == "client" then
            do
              sender <- Map.lookup "sender" jsonData
              recipient <- Map.lookup "recipient" jsonData
              Just $ do
                room <- readIORef roomRef
                name <- readIORef nameRef
                case (room, name) of
                  (Just r, Just n) ->
                      if n /= sender
                      then sendError conn "incorrect sender" jsonString
                      else do
                        s <- readMVar mstate
                        x <- forwardMessage s r recipient jsonString
                        if x then return () else sendError conn "no such recipient" jsonString
                  _ -> sendError conn "not joined yet" jsonString
          else Nothing
    case y of
      Nothing -> do
                   putStrLn $ "bad message: " ++ jsonString
                   sendError conn "bad message" jsonString
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

main :: IO ()
main = do
  mstate <- newMVar newServerState
  putStrLn $ "Starting server on port " ++ show port ++ "..."
  WS.runServer "127.0.0.1" port $ server mstate
