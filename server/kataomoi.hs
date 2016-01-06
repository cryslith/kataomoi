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
    J.Error e -> print e
    J.Ok jsonData -> do
                   let dict = J.fromJSObject jsonData
                   print [(k, T.unpack v) | (k,v) <- dict]

main :: IO ()
main = WS.runServer "18.243.0.45" 8000 server
