{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text          (Text, pack)
import           System.Environment
import           Yesod

data App = App
    { myApproot      :: Text
    , welcomeMessage :: Text
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    approot = ApprootMaster myApproot

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    App {..} <- getYesod
    setTitle "Переменные окружения"
    [whamlet|
        <p>Приветственное сообщение: #{welcomeMessage}
        <p>
            <a href=@{HomeR}>И ссылка на: @{HomeR}
    |]

main :: IO ()
main = do
    myApproot <- fmap pack $ getEnv "APPROOT"
    welcomeMessage <- fmap pack $ getEnv "WELCOME_MESSAGE"
    warp 3000 App {..}
