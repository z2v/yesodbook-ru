{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
import           HelloSub
import           Yesod

-- Давайте создадим основной сайт, который будет его вызывать.
data Master = Master
    { getHelloSub :: HelloSub
    }

mkYesod "Master" [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master

-- Опять опишем сигнатуру типа.
getHomeR :: HandlerT Master IO Html
getHomeR = defaultLayout
    [whamlet|
        <h1>Добро пожаловать на главную страницу
        <p>
            Обратите внимание, что вы также можете посетить #
            <a href=@{SubsiteR SubHomeR}>подсайт
            \ .
    |]

main = warp 3000 $ Master HelloSub
