{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/         HomeR     GET
/setname  SetNameR  GET POST
/sayhello SayHelloR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href=@{SetNameR}>Set your name
        <p>
            <a href=@{SayHelloR}>Say hello
    |]


-- Отобразить форму для ввода имени
getSetNameR :: Handler Html
getSetNameR = defaultLayout
    [whamlet|
        <form method=post>
            My name is #
            <input type=text name=name>
            . #
            <input type=submit value="Set name">
    |]

-- Достать указанное пользователем имя
postSetNameR :: Handler ()
postSetNameR = do
    -- Получить указанное имя и установить его для сессии
    name <- runInputPost $ ireq textField "name"
    setSession "name" name

    -- После того как мы получили имя, перенаправить в пункт назначения.
    -- Если пункт назначения не задан, по умолчанию направляем на домашнюю страницу
    redirectUltDest HomeR

getSayHelloR :: Handler Html
getSayHelloR = do
    -- Ищем значение имени установленное в сессии
    mname <- lookupSession "name"
    case mname of
        Nothing -> do
            -- Имя не задано, устанавливаем текущюю станицу как
            -- пункт назначения и перенаправляем на
            -- страницу задания имени - SetName
            setUltDestCurrent
            setMessage "Please tell me your name"
            redirect SetNameR
        Just name -> defaultLayout [whamlet|<p>Welcome #{name}|]

main :: IO ()
main = warp 3000 App
