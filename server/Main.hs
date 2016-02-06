import qualified Network.WebSockets as WS
import qualified Text.JSON as J
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Control.Exception as C
import Control.Monad (forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.IORef

type User = String
type UserData = WS.Connection

type ServerState = Map.Map User UserData

port = 8989

newServerState :: ServerState
newServerState = Map.empty

userList :: ServerState -> [User]
userList = Map.keys

addUser :: User -> UserData -> ServerState -> Maybe ServerState
addUser u c s = if Map.member u s then Nothing else Just $ Map.insert u c s

removeUser :: User -> ServerState -> ServerState
removeUser = Map.delete

sendUsers :: WS.Connection -> ServerState -> IO ()
sendUsers conn s = WS.sendTextData conn $ T.pack $ J.encode $
                   J.toJSObject [("type", J.showJSON "users"),
                                 ("users", J.showJSONs (userList s))]

sendAllUsers :: ServerState -> IO ()
sendAllUsers s = Map.fold (\conn m -> (m >> (sendUsers conn s))) (return ()) s

sendError :: WS.Connection -> String -> String -> IO ()
sendError conn s m = WS.sendTextData conn $ T.pack $ J.encode $
                     J.toJSObject [("type", J.showJSON "error"),
                                   ("error", J.showJSON s),
                                   ("message", J.showJSON m)]

clientMessages = ["initiate", "respond", "decrypt", "reveal", "confirm"]

server :: MVar ServerState -> WS.ServerApp
server mstate pending = do
  conn <- WS.acceptRequest pending
  nameRef <- newIORef Nothing
  forever $ do
    text <- WS.receiveData conn
    let jsonString = T.unpack text
        y = do
          jsonData <- case J.decode jsonString of
                        J.Error e -> Nothing
                        J.Ok jsonData -> Just $ Map.fromList $ J.fromJSObject jsonData
          t <- Map.lookup "type" jsonData
          if t == "join" then
            do
              name <- Map.lookup "name" jsonData
              Just $ modifyMVar_ mstate $ \s ->
                   case addUser name conn s of
                     Nothing -> do
                                  sendError conn "user already exists" jsonString
                                  return s
                     Just s' -> do
                                  writeIORef nameRef $ Just name
                                  sendAllUsers s'
                                  return s'
          else if t == "quit" then Just $ do
            name <- readIORef nameRef
            case name of
              Nothing -> return ()
              Just name' -> modifyMVar_ mstate $ \s ->
                              return (removeUser name' s)
            WS.sendClose conn (T.pack "client requested quit")
          else Nothing
    case y of
      Nothing -> putStrLn $ "undecodable string: " ++ jsonString
      Just x -> x

main :: IO ()
main = do
  mstate <- newMVar newServerState
  putStrLn $ "Starting server on port " ++ show port ++ "..."
  WS.runServer "0.0.0.0" port $ server mstate
