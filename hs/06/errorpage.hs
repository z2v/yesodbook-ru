{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

instance Yesod App where
    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "Запрошенная страница не найдена"
        toWidget [hamlet|
<h1>Not Found
<p>Приносим свои извинения за неудобства, но запрашиваемая страница не может быть найдена.
|]
    errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href=@{ErrorR}>Internal server error
            <a href=@{NotFoundR}>Not found
    |]

getErrorR :: Handler ()
getErrorR = error "Ошибка!"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 App
