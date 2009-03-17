module Control.Concurrent.Chan.Sync (
    SyncT,
    sync,
    Chan,
    newChan,
    readChan,
    writeChan,
    dupChan,
    unGetChan,
    isEmptyChan,
    getChanContents,
    writeList2Chan,
    withChan
) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import qualified Control.Concurrent.Chan as Ch
import Control.Monad (liftM2, MonadPlus)
import Control.Monad.Trans (MonadTrans(..), MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)

import Control.Monad.After

--
-- SyncT
--

newtype (Monad m) => SyncT m a = SyncT { runSyncT :: m a }

deriving instance Functor m => Functor (SyncT m)
deriving instance Monad m => Monad (SyncT m)
deriving instance MonadPlus m => MonadPlus (SyncT m)
deriving instance MonadFix m => MonadFix (SyncT m)
deriving instance MonadIO m => MonadIO (SyncT m)
deriving instance MonadReader r m => MonadReader r (SyncT m)
deriving instance MonadWriter w m => MonadWriter w (SyncT m)
deriving instance MonadState s m => MonadState s (SyncT m)
deriving instance MonadCont m => MonadCont (SyncT m)
deriving instance After m => After (SyncT m)

instance MonadTrans SyncT where
    lift = SyncT

sync :: (Monad m) => SyncT m a -> m a
sync = runSyncT

--
-- Mutex
--

type Mutex = MVar ()

newMutex     = liftIO $ newMVar ()
lock mutex   = liftIO $ takeMVar mutex >> return ()
unlock mutex = liftIO $ putMVar mutex ()

--
-- Chan
--

data Chan a = Chan { chanMutex :: Mutex, chanChan :: Ch.Chan a }

newChan :: (MonadIO m) => m (Chan a)
newChan = liftIO $ liftM2 Chan newMutex Ch.newChan

writeChan :: (MonadIO m) => Chan a -> a -> SyncT m ()
writeChan (Chan _ ch) = liftIO . Ch.writeChan ch

readChan :: (MonadIO m) => Chan a -> SyncT m a
readChan = liftIO . Ch.readChan . chanChan

dupChan :: (MonadIO m) => Chan a -> SyncT m (Chan a)
dupChan (Chan _ ch) = liftIO $ liftM2 Chan newMutex (Ch.dupChan ch)
    
unGetChan :: (MonadIO m) => Chan a -> a -> SyncT m ()
unGetChan (Chan _ ch) = liftIO . Ch.unGetChan ch

isEmptyChan :: (MonadIO m) => Chan a -> SyncT m Bool
isEmptyChan = liftIO . Ch.isEmptyChan . chanChan

getChanContents :: (MonadIO m) => Chan a -> SyncT m [a]
getChanContents = liftIO . Ch.getChanContents . chanChan

writeList2Chan :: (MonadIO m) => Chan a -> [a] -> SyncT m ()
writeList2Chan (Chan _ ch) = liftIO . Ch.writeList2Chan ch

--
-- Combinators
--

withChan :: (Monad m, MonadIO m, After m) => Chan a -> SyncT m () -> m ()
withChan (Chan mutex chan) act = sync $ (lock mutex >> act) `after` (unlock mutex)

