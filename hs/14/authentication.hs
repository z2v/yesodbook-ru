{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies,
             MultiParamTypeClasses, QuasiQuotes #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Data.Text (Text)
import Network.HTTP.Conduit (Manager, newManager, def)

data MyAuthSite = MyAuthSite
    { httpManager :: Manager
    }

mkYesod "MyAuthSite" [parseRoutes|
/ RootR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyAuthSite where
    -- Внимание! Что бы работал вход с BrowserID, вы должны здесь
    -- корректно установить адрес вашего хоста.
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth MyAuthSite where
    type AuthId MyAuthSite = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = RootR
    logoutDest _ = RootR

    authPlugins _ =
        [ authBrowserId
        , authGoogleEmail
        ]

    authHttpManager = httpManager

instance RenderMessage MyAuthSite FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler RepHtml
getRootR = do
    maid <- maybeAuthId
    defaultLayout [whamlet|
<p>Your current auth ID: #{show maid}
$maybe _ <- maid
    <p>
        <a href=@{AuthR LogoutR}>Logout
$nothing
    <p>
        <a href=@{AuthR LoginR}>Go to the login page
|]

main :: IO ()
main = do
    man <- newManager def
    warpDebug 3000 $ MyAuthSite man
