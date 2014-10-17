{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/            HomeR       GET
/set-message SetMessageR POST
|]

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        giveUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <p>Ваше сообщение: #{msg}
                        ^{pageBody pc}
            |]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{SetMessageR}>
            Моё сообщение: #
            <input type=text name=message>
            <button>Поехали
    |]

postSetMessageR :: Handler ()
postSetMessageR = do
    msg <- runInputPost $ ireq textField "message"
    setMessage $ toHtml msg
    redirect HomeR

main :: IO ()
main = warp 3000 App
