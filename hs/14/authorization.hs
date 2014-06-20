{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default         (def)
import           Data.Text            (Text)
import           Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy -- только для тестирования, не используйте в реальной жизни!!!

data App = App
    { httpManager :: Manager
    }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/admin AdminR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR

    -- имя маршрута и булево значение определяющее, является ли текущий запрос запросом на запись
    isAuthorized HomeR True = isAdmin
    isAuthorized AdminR _ = isAdmin

    -- любой может получить доступ к другим страницам
    isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Вы должны быть администратором"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authDummy]

    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>На заметку: используйте "admin" для получения административных прав.
            <p>Ваш текущий идентификатор: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Выйти
            <p>
                <a href=@{AdminR}>Перейти на страницу администрирования
            <form method=post>
                Внести изменение (только администраторы)
                \ #
                <input type=submit>
        |]

postHomeR :: Handler ()
postHomeR = do
    setMessage "Вы внесли изменения на страницу"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout
    [whamlet|
        <p>Похоже, Вы администратор!
        <p>
            <a href=@{HomeR}>Назад на домашнюю страницу
    |]

main :: IO ()
main = do
    manager <- newManager conduitManagerSettings
    warp 3000 $ App manager
