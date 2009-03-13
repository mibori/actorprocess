module PingPong where

import Control.Process

{-
    Клиент заданное число раз отсылает сообщение Ping серверу.
    Сервер на каждый раз отвечает Pong.
    Затем клиент отсылает серверу сообщение Finish
        и сервер завершает выполнение.
-}

-- старт
start = action $ do
    srv <- spawn server -- стартуем процесс сервера
    client 3 srv        -- запускаем процедуру клиента


-- сервер принимает сообщения типа Server
data Server     -- команды серверу:
    = Finish    -- завершить выполнение сервера
    | Ping (Process Client)
                -- вернуть сообщение Pong клиенту

-- процедура сервера
server :: Proc Server ()
server = do
    msg <- recv              -- ожидать сообщения
    case msg of
        Finish -> putLine "Server: received Finish"
        Ping cli -> do
            putLine "Server: received Ping"
            send cli Pong    -- отослать сообщение клиенту
            server           -- работать дальше


-- клиент от сервера получает только сообщение Pong
data Client = Pong
client
    :: Integer          -- сколько сообщений Ping отослать серверу
    -> Process Server   -- ссылка на процесс сервера
    -> Proc Client ()

client 0 srv = send srv Finish -- завершаем сервер
client n srv = do
    me <- self          -- получаем ссылку на себя
    send srv (Ping  me) -- отсылаем серверу Ping
    msg <- recv         -- ждем сообщения Pong
    case msg of
        Pong -> putLine "Client: received Pong"
    client (n - 1) srv    -- цикл


-- процедура: вывести строку
putLine :: String -> Proc t ()
putLine s = liftIO $ putStrLn s

{- Пример работы.

Prelude> :l PingPong
[1 of 1] Compiling PingPong         ( PingPong.hs, interpreted )
Ok, modules loaded: PingPong.
*PingPong>
*PingPong> start
Loading package mtl-1.1.0.2 ... linking ... done.
Loading package ActorProcess-0.1.0 ... linking ... done.
Server: received Ping
Client: received Pong
Server: received Ping
Client: received Pong
Server: received Ping
Client: received Pong
*PingPong>

-}

