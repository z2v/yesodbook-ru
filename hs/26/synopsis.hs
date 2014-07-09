{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit -- основной модуль

-- Потоковый интерфейс использует кондуиты
import Data.Conduit
import Data.Conduit.Binary (sinkFile)

import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = do
    -- Простейший запрос: просто загружаем информацию с указанного URL
    -- как ленивую ByteString.
    simpleHttp "http://www.example.com/foo.txt" >>= L.writeFile "foo.txt"

    -- Теперь используем потоковый интерфейс. Нам необходимо запускать
    -- всё внутри ResourceT, чтобы быть уверенными, что все наши
    -- соединения корректно закрываются в случае возникновения исключений
    runResourceT $ do
        -- Нам нужен Manager, который отслеживает открытые соединения.
        -- simpleHttp создаёт нового менеджера соединений при каждом
        -- запуске (т.е. соединения никогда не используются повторно)
        manager <- liftIO $ newManager conduitManagerSettings

        -- Более эффективная версия запроса с simpleHttp выше.
        -- Сначала разбираем URL в запрос
        req <- liftIO $ parseUrl "http://www.example.com/foo.txt"

        -- Теперь получаем ответ
        res <- http req manager

        -- И, наконец, записываем результат в файл
        responseBody res $$+- sinkFile "foo.txt"

        -- Сделаем это POST запросом, не следуем за перенаправлениями
        -- и принимаем любой код статуса ответа
        let req2 = req
                { method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                }
        res2 <- http req2 manager
        responseBody res2 $$+- sinkFile "post-foo.txt"

