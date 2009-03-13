module Control.Process.Delay(threadDelayInteger, readChanDelay, readChanNow) where

import Control.Concurrent
import Control.Monad (replicateM_)
import Foreign.Storable

divt :: Integer -> (Integer, Int)
divt long = (ii, fromInteger i) where (ii, i) = divMod long $ toInteger (maxBound :: Int) 

threadDelayInteger :: Integer -> IO ()
threadDelayInteger n = replicateM_ (fromInteger ii) (threadDelay maxBound) >> threadDelay i where
    (ii, i) = divt n

readChanDelay :: Chan ch -> Integer -> IO (Maybe ch)
readChanDelay ch n = do
    v <- newEmptyMVar
    r <- forkIO $ putMVar v . Just =<< readChan ch
    forkIO $ threadDelayInteger n >> killThread r >> putMVar v Nothing
    takeMVar v

readChanNow :: Chan ch -> IO (Maybe ch)
readChanNow ch = do
    b <- isEmptyChan ch
    if b then return Nothing
        else return . Just =<< readChan ch
