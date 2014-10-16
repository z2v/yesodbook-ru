{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Data.Time (getCurrentTime)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        giveUrlRenderer [hamlet|
            $doctype 5

            <html>
                <head>
                    <title>#{title}
                    ^{headTags}
                <body>
                    $maybe msg <- mmsg
                        <div #message>#{msg}
                    ^{bodyTags}
        |]

getHomeR :: Handler Html
getHomeR = do
    now <- liftIO getCurrentTime
    setMessage $ toHtml $ "В прошлый вы заходили: " ++ show now
    defaultLayout [whamlet|<p>Попробуйте обновить страницу|]

main :: IO ()
main = warp 3000 App

