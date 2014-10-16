{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]
instance Yesod App

getHomeR = defaultLayout $ do
    setTitle "My Page Title"
    toWidget [lucius| h1 { color: green; } |]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            $(function() {
                $("h1").click(function(){
                    alert("Вы кликнули на заголовок!");
                });
            });
        |]
    toWidgetHead
        [hamlet|
            <meta name=keywords content="некоторые ключевые слова примера">
        |]
    toWidget
        [hamlet|
            <h1> Это один способ вставки контента
        |]
    [whamlet|<h2>А это другой |]
    toWidgetBody
        [julius|
            alert("Это вставляется в само тело");
        |]

main = warp 3000 App
