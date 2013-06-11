{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, GADTs,
             TemplateHaskell, MultiParamTypeClasses, FlexibleContexts #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import Network.Mail.Mime
import qualified Data.Text.Lazy.Encoding
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Data.Maybe (isJust)
import Control.Monad (join)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    email Text
    password Text Maybe -- Пароль может быть ещё не задан
    verkey Text Maybe -- Используется для сброса пароля
    verified Bool
    UniqueUser email
|]

data MyEmailApp = MyEmailApp Connection

mkYesod "MyEmailApp" [parseRoutes|
/ RootR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyEmailApp where
    -- Электронные письма будут содержать ссылки, так что убедитесь, что включили approot,
    -- чтобы ссылки были правильными!
    approot = ApprootStatic "http://localhost:3000"

instance RenderMessage MyEmailApp FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Установка Persistent
instance YesodPersist MyEmailApp where
    type YesodPersistBackend MyEmailApp = SqlPersist
    runDB f = do
        MyEmailApp conn <- getYesod
        runSqlConn f conn

instance YesodAuth MyEmailApp where
    type AuthId MyEmailApp = UserId

    loginDest _ = RootR
    logoutDest _ = RootR
    authPlugins _ = [authEmail]

    -- Необходимо найти UserId по заданному адресу электронной почты.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- свежедобавленый пользователь
                Right userid -> userid -- существующий пользователь

    authHttpManager = error "Email doesn't need an HTTP manager"

-- Здесь весь код работающий с электронной почтой
instance YesodAuthEmail MyEmailApp where
    type AuthEmailId MyEmailApp = UserId

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                }
    getEmail = runDB . fmap (fmap userEmail) . get

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
main = withSqliteConn "email.db3" $ \conn -> do
    runStderrLoggingT $ runSqlConn (runMigration migrateAll) conn
    warpDebug 3000 $ MyEmailApp conn
