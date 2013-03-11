{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
    ( http, parseUrl, withManager, RequestBody (RequestBodyLBS)
    , requestBody, method, Response (..)
    )
import Data.Aeson (Value (Object, String))
import Data.Aeson.Parser (json)
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, (.=), object)

main :: IO ()
main = withManager $ \manager -> do
    value <- liftIO makeValue
    -- Нам надо знать размер тела запроса, поэтому преобразуем его к
    -- ByteString
    let valueBS = encode value
    req' <- liftIO $ parseUrl "http://localhost:3000/"
    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    Response status version headers body <- http req manager
    resValue <- body $$+- sinkParser json
    liftIO $ handleResponse resValue

-- Фунция бизнес-логики приложения, создающая значение для запроса
makeValue :: IO Value
makeValue = return $ object
    [ ("foo" .= ("bar" :: String))
    ]

-- Функция бизнес-логики приложения, обрабатывающая ответ от сервера
handleResponse :: Value -> IO ()
handleResponse = print
