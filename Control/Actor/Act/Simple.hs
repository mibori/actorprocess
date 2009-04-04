module Control.Actor.Act.Simple (
    Act
) where

import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan, unGetChan)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get)

import Control.Actor.Classes

data SimpleProcess msg = SimpleProcess (Chan msg)

newtype Act msg a = Act { runAct :: StateT (Chan msg) IO a }
    deriving (Functor, Monad, MonadState (Chan msg), MonadIO)

type instance Process msg = SimpleProcess msg

type instance ActMsg (Act msg) = msg

instance Action (Act a) where
    action (Act act) = do
        ch <- newChan
        evalStateT act ch

    spawn (Act act) = liftIO $ do
        ch  <- newChan
        tid <- forkIO $ evalStateT act ch
        return $ SimpleProcess ch

    self = liftM SimpleProcess get

instance Sender (Act state) msg where
    send     (SimpleProcess ch) = liftIO . writeChan ch
    sendBack (SimpleProcess ch) = liftIO . unGetChan ch

instance Receiver (Act msg msg) where
    recv = get >>= liftIO . readChan

