{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.IORef
import           Yesod

data App = App
    { visitors :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    visitorsRef <- fmap visitors getYesod
    visitors <-
        liftIO $ atomicModifyIORef visitorsRef $ \i ->
        (i + 1, i + 1)
    defaultLayout
        [whamlet|
            <p>Добро пожаловать! Вы посетитель №#{visitors}.
        |]

main :: IO ()
main = do
    visitorsRef <- newIORef 0
    warp 3000 App
        { visitors = visitorsRef
        }
