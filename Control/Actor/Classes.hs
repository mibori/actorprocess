module Control.Actor.Classes (
    Process,
    ActMsg,
    Action(..),
    Receiver(..),
    Sender(..)
) where

type family Process act msg

type family ActMsg act

class Action act where
    action :: act a -> IO a
    spawn  :: act () -> IO (Process act (ActMsg act))
    self   :: act (Process act (ActMsg act))

class Receiver r where
    recv :: r

class Sender s msg where
    send     :: Process s msg -> msg -> s ()
    sendBack :: Process s msg -> msg -> s ()

