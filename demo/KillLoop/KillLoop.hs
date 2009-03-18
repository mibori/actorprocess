module KillLoop where

import Control.Process

loop :: Proc () ()
loop = do
    liftIO $ putStrLn "Hello world"
    loop            -- бесконечный цикл

f :: Proc () ()
f = do
    p <- spawn loop -- создать процесс из loop
    delay 5000000   -- подождать пять секунд
    kill p          -- прибить процесс

start :: IO ()
start = action f

