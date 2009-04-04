import Control.Monad.Trans (liftIO)
import Control.Actor
import Control.Actor.Act.Simple

putLine s = liftIO $ putStrLn s

data Server = Finish | Ping (Process Client)
data Client = Pong

start = action $ do
    srv <- liftIO $ spawn server
    client 3 srv

server :: Act Server ()
server = do
    msg <- recv
    case msg of
        Finish -> putLine "Server: received Finish"
        Ping cli -> do
            putLine "Server: received Ping"
            send cli Pong
            server

client :: Integer -> Process Server -> Act Client ()
client 0 srv = send srv Finish
client n srv = do
    me <- self
    send srv (Ping me)
    msg <- recv
    case msg of
        Pong -> putLine "Client: received Pong"
    client (n - 1) srv

