module Control.Process.Delay(threadDelayInteger, readChanDelay, readChanNow) where

import Control.Concurrent
import Control.Exception (block)
import Control.Monad (replicateM_)
import Foreign.Storable

divt :: Integer -> (Integer, Int)
divt long = (ii, fromInteger i) where (ii, i) = divMod long $ toInteger (maxBound :: Int) 

threadDelayInteger :: Integer -> IO ()
threadDelayInteger n = replicateM_ (fromInteger ii) (threadDelay maxBound) >> threadDelay i where
    (ii, i) = divt n

readChanDelay :: Integer -> Chan ch -> IO (Maybe ch)
readChanDelay n ch = do
    self <- myThreadId
    block $ do
        killer <- forkIO $ threadDelayInteger n >> throwTo self (userError "timeout")
        result <- fmap Just (readChan ch) `catch` (\_ -> return Nothing)
        killThread killer
        return result

readChanNow :: Chan ch -> IO (Maybe ch)
readChanNow ch = do
    b <- isEmptyChan ch
    if b then return Nothing
         else fmap Just (readChan ch)
