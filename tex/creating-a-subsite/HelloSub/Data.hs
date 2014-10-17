{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module HelloSub.Data where

import Yesod

-- У подсайтов, также как и у основного сайта, есть основной тип данных.
data HelloSub = HelloSub

-- Тут аналог знакомого нам mkYesod, с одним дополнительным параметром.
-- Мы обсудим это позже.
mkYesodSubData "HelloSub" [parseRoutes|
/ SubHomeR GET
|]
