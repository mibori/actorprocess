module Control.Process.Delay(threadDelayInteger, readChanDelay, readChanNow) where

import Control.Concurrent
import Foreign.Storable

divt :: Integer -> (Integer, Int)
divt long = (ii, fromInteger i) where (ii, i) = divMod long $ toInteger (maxBound :: Int) 

threadDelayInteger :: Integer -> IO ()
threadDelayInteger n = repeatThreadDelay ii >> threadDelay i where
    (ii, i) = divt n
    repeatThreadDelay 0 = return ()
    repeatThreadDelay n = threadDelay maxBound >> repeatThreadDelay (n - 1)

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
