{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links

getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Перейти на страницу 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Перейти на страницу 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Вернуться к началу!|]

main = warp 3000 Links
