{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module HelloSub
    ( module HelloSub.Data
    , module HelloSub
    ) where

import           HelloSub.Data
import           Yesod

-- Опишем сигнатуру типа для обработчика.
getSubHomeR :: Yesod master => HandlerT HelloSub (HandlerT master IO) Html
getSubHomeR = lift $ defaultLayout [whamlet|Добро пожаловать на подсайт!|]

instance Yesod master => YesodSubDispatch HelloSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
