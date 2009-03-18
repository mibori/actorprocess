module Control.Actor.Classes (
    Process,
    ActMsg,
    Action(..),
    Receiver(..),
    Sender(..)
) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.State
import Data.Dynamic
import Data.Maybe

type family Process act msg

type family ActMsg act

class Action act msg where
    action :: act a -> IO a
    spawn  :: act () -> IO (Process act (ActMsg act))
    self   :: act (Process act (ActMsg act))

class Receiver r where
    recv :: r

class Sender s msg where
    send     :: Process s msg -> msg -> s ()
    sendBack :: Process s msg -> msg -> s ()

