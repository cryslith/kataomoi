import qualified Network.WebSockets as WS
import qualified Text.JSON as J
import qualified Data.Map as Map
import qualified Data.Text as T

type User = String
type UserData = ()

type ServerState = Map.Map User UserData

server :: WS.ServerApp
server pending = do
  conn <- WS.acceptRequest pending
  text <- WS.receiveData conn
  let jsonString = T.unpack text
  let result = J.decode jsonString
  case result of
    Error e => print e
    Ok jsonData => 
