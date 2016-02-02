import qualified Network.WebSockets as WS
import qualified Text.JSON as J
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Control.Exception as C
import Control.Monad (forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

type User = String
type UserData = ()

type ServerState = Map.Map User UserData

newServerState :: ServerState
newServerState = Map.empty

userList :: ServerState -> [User]
userList = Map.keys

addUser :: User -> UserData -> ServerState -> ServerState
addUser = Map.insert

server :: MVar ServerState -> WS.ServerApp
server mstate pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    text <- WS.receiveData conn
    let jsonString = T.unpack text
        y = do
          jsonData <- case J.decode jsonString of
                        J.Error e -> Nothing
                        J.Ok jsonData -> Just jsonData
          t <- Map.lookup "type" jsonData
          if t == "users" then
              Just $ do
                s <- readMVar mstate
                WS.sendTextData conn $ T.pack $ J.encode $ userList s
          else if t == "hello" then
                   do
                     name <- Map.lookup "name" jsonData
                     Just $ modifyMVar_ mstate $ \s -> do
                          let s' = addUser name () s
                          WS.sendTextData conn $ T.pack $ J.encode $ userList s'
                          return s'
               else Nothing
    case y of
      Nothing -> putStrLn $ "undecodable string: " ++ jsonString
      Just x -> x

main :: IO ()
main = do
  mstate <- newMVar newServerState
  WS.runServer "0.0.0.0" 8000 $ server mstate
