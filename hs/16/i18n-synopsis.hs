{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies,
    MultiParamTypeClasses #-}
import Yesod

data I18N = I18N

mkMessage "I18N" "messages" "en"

plural :: Int -> String -> String -> String -> String
plural 1 x _ _ = x
plural n _ y z = if n < 5 then y else z

showInt :: Int -> String
showInt = show

instance Yesod I18N

instance RenderMessage I18N FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "I18N" [parseRoutes|
/ RootR GET
/buy BuyR GET
/lang LangR POST
|]

getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
<h1>_{MsgHello}
<form action=@{BuyR}>
    _{MsgEnterItemCount}
    <input type=text name=count>
    <input type=submit value=_{MsgPurchase}>
<form action=@{LangR} method=post>
    _{MsgSwitchLanguage}
    <select name=lang>
        <option value=en>English
        <option value=ru>Russian
    <input type=submit value=_{MsgSwitch}>
|]

getBuyR :: Handler RepHtml
getBuyR = do
    count <- runInputGet $ ireq intField "count"
    defaultLayout [whamlet|
<p>_{MsgItemCount count}
|]

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect RootR

main :: IO ()
main = warpDebug 3000 I18N
