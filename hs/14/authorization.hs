{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies,
             MultiParamTypeClasses, QuasiQuotes #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Dummy -- только для тестирования, не используйте в реальной жизни!!!
import Data.Text (Text)
import Network.HTTP.Conduit (Manager, newManager, def)

data MyAuthSite = MyAuthSite
    { httpManager :: Manager
    }

mkYesod "MyAuthSite" [parseRoutes|
/ RootR GET POST
/admin AdminR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyAuthSite where
    authRoute _ = Just $ AuthR LoginR

    -- имя маршрута и булево значение определяющее, является ли текущий запрос запросом на запись
    isAuthorized RootR True = isAdmin
    isAuthorized AdminR _ = isAdmin

    -- любой может получить доступ к другим страницам
    isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "You must be an admin"

instance YesodAuth MyAuthSite where
    type AuthId MyAuthSite = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = RootR
    logoutDest _ = RootR

    authPlugins _ = [authDummy]

    authHttpManager = httpManager

instance RenderMessage MyAuthSite FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler RepHtml
getRootR = do
    maid <- maybeAuthId
    defaultLayout [whamlet|
<p>Note: Log in as "admin" to be an administrator.
<p>Your current auth ID: #{show maid}
$maybe _ <- maid
    <p>
        <a href=@{AuthR LogoutR}>Logout
<p>
    <a href=@{AdminR}>Go to admin page
<form method=post>
    Make a change (admins only)
    \ #
    <input type=submit>
|]

postRootR :: Handler ()
postRootR = do
    setMessage "You made some change to the page"
    redirect RootR

getAdminR :: Handler RepHtml
getAdminR = defaultLayout [whamlet|
<p>I guess you're an admin!
<p>
    <a href=@{RootR}>Return to homepage
|]

main :: IO ()
main = do
    manager <- newManager def
    warpDebug 3000 $ MyAuthSite manager

