module Database where

import Control.Process

{-
    start запускает сервер, хранящий базу [(ключ, значение)].
    также запускает клиент, позволяющий добавлять и просматривать
        значения по ключу.
-}


start = action $ do
    srv <- spawn $ server []
    client srv

-- Серверная сторона
-- Сообщения для сервера, от клиента
data Server
    = Alloc String (Process Client)
        -- зарегистрировать новый ключ
    | Value String (Process Client)
        -- передача значения ключа, после Alloc
    | Lookup String (Process Client)
        -- найти значение ключа
        deriving Show

server db = do
    msg <- recv -- ждем сообщения от клиента
    case msg of

        -- зарегистрировать новый ключ
        Alloc key cli ->
            case lookup key db of
                Just _ -> do           -- ключ есть
                    send cli Allocated -- сообщаем это клиенту
                    server db
                Nothing -> do     -- ключа нет
                    send cli Free -- сообщаем это клиенту 
                    msg' <- recv  -- ожидаем значения
                    case msg' of
                        Value val cli' -> 
                            if cli == cli' -- Alloc и Value посылал один и тот же клиент?
                                then server ((key, val): db) -- Да
                                else server db               -- Нет
                        -- ничего, кроме Value
                        _ -> server db

        -- узнать значение ключа
        Lookup key cli -> do
            send cli $ IsValue $ lookup key db -- отправить результат
            server db

        -- команда Value только после Alloc
        _ -> server db


-- Клиентская часть
-- Сообщения, посылаемые клиенту от сервера
data Client
    = Allocated
        -- после Alloc. Такой ключ уже есть
    | Free
        -- после Alloc. Такого ключа еще нет
    | IsValue (Maybe String)
        -- после Lookup. Just значение, если ключ есть.
        deriving Show

-- печать строчки
putS = liftIO . putStr

-- запросить строчку у пользователя
getL = liftIO getLine

-- процедура клиента
client srv = do
    putS "(i)nsert / (l)ookup >"
    s <- getL
    me <- self  -- ссылка на себя
    case s of   -- обработка команды от пользователя
        -- создать новые ключ-значение
        "i" -> do
            putS "Key >"
            key <- getL             -- вводим ключ
            send srv $ Alloc key me -- командуем серверу
            ans <- recv             -- ждем ответа
            case ans of             -- ответ пришел
                Free -> do          -- нет такого ключа
                    putS "Value >"
                    val <- getL     -- вводим значение
                    send srv $ Value val me -- отправляем серверу
                Allocated ->        -- такой ключ есть
                    putS "Key allocated.\n" -- сообщаем пользователю
            client srv

        -- запросить значение по ключу
        "l" -> do
            putS "Key >"
            key <- getL                 -- вводим ключ
            send srv $ Lookup key me    -- запрос серверу
            ans <- recv        -- ожидаем ответ от сервера
            case ans of        -- ответ пришел
                IsValue (Just val) ->    -- значение найдено
                    putS ("Value is " ++ val ++ ". \n")
                IsValue Nothing ->       -- значение не найдено
                    putS ("Value not found.\n")
                _ ->
                    return ()       -- никакое другое сообщение от сервера
            client srv

        -- никакая другая команда
        _ -> client srv


{-  Пример использования

Prelude> :l ./Database.hs
[1 of 1] Compiling Database         ( Database.hs, interpreted )
Ok, modules loaded: Database.
*Database>
*Database> start
Loading package mtl-1.1.0.2 ... linking ... done.
Loading package ActorProcess-0.1.0 ... linking ... done.
(i)nsert / (l)ookup >i
Key >Mibori
Value >Shante
(i)nsert / (l)ookup >l
Key >Mibori
Value is Shante.
(i)nsert / (l)ookup >l
Key >Mib
Value not found.
(i)nsert / (l)ookup >i
Key >Mibori
Key allocated.
(i)nsert / (l)ookup >

-}

