{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Set         (member)
import           Data.Text        (Text)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/unprotected UnprotectedR GET
/admin1 Admin1R GET !admin
/admin2 Admin2R GET !admin
/admin3 Admin3R GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR
    isAuthorized route _writable
        | "admin" `member` routeAttrs route = do
            muser <- maybeAuthId
            case muser of
                Nothing -> return AuthenticationRequired
                Just ident
                    -- Просто хак, так как используем модуль-заглушку
                    | ident == "admin" -> return Authorized
                    | otherwise -> return $ Unauthorized "Только для администраторов"
        | otherwise = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Поддельный экземляр YesodAuth для плагина-заглушки авторизации
instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authPlugins _ = [authDummy]
    maybeAuthId = lookupSession credsKey
    authHttpManager = error "отсутствует менеджер http"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Домашняя страница атрибутов маршрута"
    [whamlet|
        <p>
            <a href=@{UnprotectedR}>Unprotected
        <p>
            <a href=@{Admin1R}>Admin 1
        <p>
            <a href=@{Admin2R}>Admin 2
        <p>
            <a href=@{Admin3R}>Admin 3
    |]

getUnprotectedR, getAdmin1R, getAdmin2R, getAdmin3R :: Handler Html
getUnprotectedR = defaultLayout [whamlet|Unprotected|]
getAdmin1R = defaultLayout [whamlet|Admin1|]
getAdmin2R = defaultLayout [whamlet|Admin2|]
getAdmin3R = defaultLayout [whamlet|Admin3|]

main :: IO ()
main = warp 3000 App
