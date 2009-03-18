{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Control.Monad.Trans
import Control.Actor
import Control.Actor.Act.Var
import Data.Typeable

putLine s = liftIO $ putStrLn s

data Finish = Finish deriving Typeable
data Ping   = Ping (Process Act Client) deriving Typeable
data Client = Pong deriving Typeable

start = action $ do
    srv <- liftIO $ spawn server
    client 3 srv

server = finish `mplus` ping
client = pong

finish :: Act ()
finish = do
    Finish <- recv
    putLine "Server: received Finish"


ping :: Act ()
ping = do
    Ping client <- recv
    putLine "Server: received Ping"
    client ! Pong
    server


pong :: Integer -> (Process Act msg) -> Act ()
pong 0 srv = send srv Finish
pong n srv = do
    me <- self
    srv ! Ping me
    Pong <- recv
    putLine "Client: received Pong"
    pong (n - 1) srv

