{-# LANGUAGE QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
import Yesod

-- У дочерних сайтов, также как и у главных, есть основной тип данных.
data HelloSub = HelloSub

-- Тут аналог знакомого нам mkYesod, с одним дополнительным параметром.
-- Мы обсудим это позже.
mkYesodSub "HelloSub" [] [parseRoutes|
/ SubRootR GET
|]

-- Опишем сигнатуру типа для обработчика.
getSubRootR :: Yesod master => GHandler HelloSub master RepHtml
getSubRootR = defaultLayout [whamlet|Welcome to the subsite!|]

-- Давайте создадим главный сайт, который будет его вызывать.
data Master = Master
    { getHelloSub :: HelloSub
    }

mkYesod "Master" [parseRoutes|
/ RootR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master

-- Опять опишем сигнатуру типа.
getRootR :: GHandler sub Master RepHtml -- так же могли бы заменить sub на Master
getRootR = defaultLayout [whamlet|
<h1>Добро пожаловать на главную страницу
<p>
    Обратите внимание, что вы также можете посетить #
    <a href=@{SubsiteR SubRootR}>дочерний сайт
    \ .
|]

main = warpDebug 3000 $ Master HelloSub
