{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (Response, responseLBS, Application, requestBody)
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Data.Aeson.Parser (json)
import Data.Conduit.Attoparsec (sinkParser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode, object, (.=))
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Conduit (ResourceT, ($$))
import Control.Exception.Lifted (handle)

main :: IO ()
main = run 3000 app

app :: Application
app req = handle invalidJson $ do
    value <- requestBody req $$ sinkParser json
    newValue <- liftIO $ modValue value
    return $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> ResourceT IO Response
invalidJson ex = return $ responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]

-- Бизнес-логика будет находиться в этой функции
modValue :: Value -> IO Value
modValue = return
