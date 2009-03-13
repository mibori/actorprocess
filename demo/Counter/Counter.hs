module Counter where

import Control.Process

{-
    При помощи startServer st запускается сервер с начальным стоянием st.
    startServer возвращает ссылку link на процесс сервера.
    При помощи inc link, dec link, set link val можно инкрементировать
        декрементировать и просто устанваливать новое значение
        состояния сервера.
    При помощи getVal link это стояние можно прочитать.
    Завершить работу сервера можно через kill link

-}

-- Интерфейс:
-- запустить сервер и получить ссылку на его процесс
startServer st = action $ spawn $ server st

-- инкремент состояния сервера
inc srv = sendIO srv Inc

-- декремент состояния сервера
dec srv = sendIO srv Dec

-- установить серверу состояние
set srv st = sendIO srv $ Value st

-- получить от сервера значение состояние
getVal srv = action $ do
    me <- self
    send srv $ GetValue me
    recv

-- Сервер:
-- серверная часть принимает сообщения типа Server
data Server
    = Inc   -- инкремент состояния сервера
    | Dec   -- декремент состояния сервера
    | GetValue (Process Integer)
            -- получить состояние сервера
    | Value Integer
            -- установить состояние сервера
        deriving Show

-- процедура сервера
server st = do
    msg <- recv
    liftIO $ putStrLn ("Server: msg = " ++ show msg)
    case msg of
        Inc -> server (st + 1)
        Dec -> server (st - 1)
        GetValue cli -> do
            send cli st
            server st
        Value newSt -> server newSt


{- Пример работы:

Prelude> :l Counter
[1 of 1] Compiling Counter          ( Counter.hs, interpreted )
Ok, modules loaded: Counter.
*Counter> s <- startServer 4
Loading package mtl-1.1.0.2 ... linking ... done.
Loading package ActorProcess-0.1.0 ... linking ... done.
*Counter> getVal s
Server: msg = GetValue Process ThreadId 29
4
*Counter> inc s
Server: msg = Inc
*Counter> getVal s
Server: msg = GetValue Process ThreadId 32
5
*Counter>

-}

