module IOActions where

import Control.Process

main1 :: IO ()
main1 = f1

f1 :: IO ()
f1 = do
    s <- getLine
    putStrLn ("Output: " ++ s)

main2 :: IO ()
main2 = action f2

f2 :: Proc () ()
f2 = do
    s <- liftIO getLine
    liftIO $ putStrLn ("Output: " ++ s)

