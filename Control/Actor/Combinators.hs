module Control.Actor.Combinators (
    sendMe,
    sendMeBack,
    (!)
) where

import Control.Actor.Classes

sendMe msg     = self >>= flip send msg
sendMeBack msg = self >>= flip sendBack msg

(!) :: (Sender s msg) => Process s msg -> msg -> s ()
(!) = send

