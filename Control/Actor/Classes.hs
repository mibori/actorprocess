module Control.Actor.Classes (
    Process,
    ActMsg,
    Action(..),
    Receiver(..),
    Sender(..)
) where

type family Process msg

type family ActMsg act

class Action act where
    action :: act a -> IO a
    spawn  :: act () -> IO (Process (ActMsg act))
    self   :: act (Process (ActMsg act))

class Receiver r where
    recv :: r

class Sender s msg where
    send     :: Process msg -> msg -> s ()
    sendBack :: Process msg -> msg -> s ()

