module Control.Process.Action where

class Monad m => Action m where
    action :: m a -> IO a

instance Action IO where
    action = id