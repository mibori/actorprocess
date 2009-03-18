module Control.Actor.Act.Var (
    Act
) where

import Control.Concurrent (forkIO)
import Control.Monad (MonadPlus, when, liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Maybe (MaybeT, runMaybeT)
import Control.Monad.State (MonadState, StateT, evalStateT, get)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)

import Control.Concurrent.Chan.Sync (Chan, sync, newChan, readChan, writeChan, unGetChan, withChan)
import Control.Monad.After (After)

import Control.Actor.Classes
import Control.Actor.Combinators

data VarProcess = VarProcess (Chan Dynamic)

newtype Act a = Act { runAct :: MaybeT (StateT (Chan Dynamic) IO) a }
    deriving (Functor, Monad, MonadState (Chan Dynamic), MonadIO, MonadPlus, After)

type instance Process Act msg = VarProcess

type instance ActMsg Act = Dynamic

instance Action Act msg where
    action (Act act) = do
        ch <- newChan
        Just v <- evalStateT (runMaybeT act) ch -- Too dangerous!!
        return v

    spawn (Act act) = liftIO $ do
        ch <- newChan
        tid <- forkIO $ evalStateT (runMaybeT act) ch >> return () -- for a while
        return $ VarProcess ch

    self = liftM VarProcess get

instance (Typeable msg) => Sender Act msg where
    send     (VarProcess ch) = sync . writeChan ch . toDyn
    sendBack (VarProcess ch) = sync . unGetChan ch . toDyn

instance (Typeable msg) => Receiver (Act msg) where
    recv = do
        ch <- get
        withChan ch $ do
            dyn <- readChan ch
            let valM = fromDynamic dyn
            when (isNothing valM) $ unGetChan ch dyn
            Just val <- return valM
            return val

