{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession as CS

data SessionExample = SessionExample

mkYesod "SessionExample" [parseRoutes|
/ Root GET POST
|]

getRoot :: Handler RepHtml
getRoot = do
    sess <- getSession
    hamletToRepHtml [hamlet|
<form method=post>
    <input type=text name=key>
    <input type=text name=val>
    <input type=submit>
<h1>#{show sess}
|]

postRoot :: Handler ()
postRoot = do
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
    case mval of
        Nothing -> deleteSession key
        Just val -> setSession key val
    liftIO $ print (key, mval)
    redirect Root

instance Yesod SessionExample where
    -- Устанавливаем таймаут сессии в 1 минуту, чтобы облегчить тестирование
    makeSessionBackend _ = do
        key <- CS.getKey CS.defaultKeyFile
        return $ Just $ clientSessionBackend key 1

instance RenderMessage SessionExample FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warpDebug 3000 SessionExample

