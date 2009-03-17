module Control.Monad.After (After(..)) where

import Control.Exception (finally)
import Control.Monad.Maybe
import Control.Monad (mplus)

class After after where
    after :: after a -> after b -> after a

instance After IO where
    after = finally

instance (Monad m) => After (MaybeT m) where
    m1 `after` m2 = m1 `mplus` (MaybeT $ runMaybeT m2 >> return Nothing)

