{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses #-}

import Yesod

data Messages = Messages

mkYesod "Messages" [parseRoutes|
/ RootR GET
/set-message SetMessageR POST
|]

instance Yesod Messages where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <p>Your message was: #{msg}
        ^{pageBody pc}
|]

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
<form method=post action=@{SetMessageR}>
    My message is: #
    <input type=text name=message>
    <input type=submit>
|]

postSetMessageR :: Handler ()
postSetMessageR = do
    msg <- runInputPost $ ireq textField "message"
    setMessage $ toHtml msg
    redirect RootR

main :: IO ()
main = warpDebug 3000 Messages

