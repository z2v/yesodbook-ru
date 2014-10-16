{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/      HomeR  GET
|]

instance Yesod App where

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "toWidgetHead и toWidgetBody"
    toWidgetBody
        [hamlet|<script src=/included-in-body.js>|]
    toWidgetHead
        [hamlet|<script src=/included-in-head.js>|]

main :: IO ()
main = warp 3001 App
