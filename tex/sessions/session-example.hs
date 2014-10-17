{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession   as CS
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

getHomeR :: Handler Html
getHomeR = do
    sess <- getSession
    defaultLayout
        [whamlet|
            <form method=post>
                <input type=text name=key>
                <input type=text name=val>
                <input type=submit>
            <h1>#{show sess}
        |]

postHomeR :: Handler ()
postHomeR = do
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
    case mval of
        Nothing -> deleteSession key
        Just val -> setSession key val
    liftIO $ print (key, mval)
    redirect HomeR

instance Yesod App where
    -- Устанавливаем тайм-аут сессии в 1 минуту, чтобы облегчить тестирование
    makeSessionBackend _ = do
        backend <- defaultClientSessionBackend 1 "keyfile.aes"
        return $ Just backend

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warp 3000 App
