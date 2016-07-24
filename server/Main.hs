import qualified Network.WebSockets as WS
import qualified Text.JSON as J
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Control.Exception as C
import Control.Monad (forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.IORef

type User = String
type PublicKey = String
type UserData = (WS.Connection, PublicKey)

getConn :: UserData -> WS.Connection
getConn = fst

getKey :: UserData -> PublicKey
getKey = snd

type ServerState = Map.Map User UserData

port = 8989

newServerState :: ServerState
newServerState = Map.empty

userList :: ServerState -> [(User, PublicKey)]
userList = Map.toList . Map.map (\(_, key) -> key)

changeState ::  MVar ServerState -> (ServerState -> IO ServerState)-> IO ()
changeState mstate f = do
  modifyMVar_ mstate f
  state' <- readMVar mstate
  sendAllUsers state'

addUser :: User -> UserData -> ServerState -> Maybe ServerState
addUser u c s = if Map.member u s then Nothing else Just $ Map.insert u c s

removeUser :: User -> ServerState -> ServerState
removeUser = Map.delete

send :: J.JSON a => WS.Connection -> a -> IO ()
send conn = WS.sendTextData conn . T.pack . J.encode

sendUsers :: WS.Connection -> ServerState -> IO ()
sendUsers conn s = send conn $
  J.toJSObject [("type", J.showJSON "users"),
                ("users", J.JSObject (J.toJSObject $
                   map (\(user, key) -> (user, J.showJSON key)) $
                     userList s))]

sendAllUsers :: ServerState -> IO ()
sendAllUsers s = Map.fold (\(conn, _) m ->
                             (m >> (sendUsers conn s))) (return ()) s

forwardMessage :: ServerState -> User -> String -> IO Bool
forwardMessage s user t =
  case Map.lookup user s of
    Just (conn, _) -> do
      WS.sendTextData conn $ T.pack $ t
      return True
    Nothing -> return False

sendWelcome :: WS.Connection -> String -> IO ()
sendWelcome conn n = send conn $
                     J.toJSObject [("type", J.showJSON "welcome"),
                                   ("name", J.showJSON n)]

sendUnavailable :: WS.Connection -> String -> IO ()
sendUnavailable conn n = send conn $
                         J.toJSObject [("type", J.showJSON "unavailable"),
                                       ("name", J.showJSON n)]

sendError :: WS.Connection -> String -> String -> IO ()
sendError conn s m = send conn $
                     J.toJSObject [("type", J.showJSON "error"),
                                   ("error", J.showJSON s),
                                   ("message", J.showJSON m)]

server :: MVar ServerState -> WS.ServerApp
server mstate pending = do
  conn <- WS.acceptRequest pending
  nameRef <- newIORef Nothing
  flip C.finally (disconnect conn nameRef) $ forever $ do
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
              pubkey <- Map.lookup "pubkey" jsonData
              Just $ if usernameOK name then
                changeState mstate $ \s ->
                  case addUser name (conn, pubkey) s of
                    Nothing -> do
                                 sendUnavailable conn name
                                 return s
                    Just s' -> do
                                 n <- readIORef nameRef
                                 writeIORef nameRef $ Just name
                                 sendWelcome conn name
                                 return $ case n of
                                            Nothing -> s'
                                            Just n' -> removeUser n' s'
               else sendError conn "invalid username" jsonString
          else if t == "client" then
            do
              sender <- Map.lookup "sender" jsonData
              recipient <- Map.lookup "recipient" jsonData
              Just $ do
                s <- readMVar mstate
                x <- forwardMessage s recipient jsonString
                if x then return () else sendError conn "no such recipient"
                                                   jsonString
          else if t == "quit" then Just $ disconnect conn nameRef
          else Nothing
    case y of
      Nothing -> do
                   putStrLn $ "bad message: " ++ jsonString
                   sendError conn "bad message" jsonString
      Just x -> x
  where
    disconnect conn nameRef = do
      WS.sendClose conn (T.pack "quit")
      name <- readIORef nameRef
      case name of
        Nothing -> return ()
        Just name' -> changeState mstate $ \s -> return $ removeUser name' s

usernameOK :: String -> Bool
usernameOK username =
  length username >= 1 &&
  length username <= 8 &&
  all (\c -> 'a' <= c && c <= 'z' ||
             'A' <= c && c <= 'Z' ||
             '0' <= c && c <= '9') username

main :: IO ()
main = do
  mstate <- newMVar newServerState
  putStrLn $ "Starting server on port " ++ show port ++ "..."
  WS.runServer "0.0.0.0" port $ server mstate
