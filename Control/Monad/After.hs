module Control.Monad.After (After(..)) where

import Control.Exception (finally)
import Control.Monad.Maybe
import Control.Monad (mplus)

class After after where
    after :: after a -> after b -> after a

instance After IO where
    after = finally

--
-- Simple and wrong implementation
-- TODO: implement correct version (IO etc.)
--
instance (Monad m) => After (MaybeT m) where
    m1 `after` m2 = do
        r1 <- m1
        m2
        return r1

