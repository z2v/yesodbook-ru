{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod
import Network.HTTP.Types (encodePath)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Arrow ((***))
import Data.Monoid (mappend)

data Slash = Slash

mkYesod "Slash" [parseRoutes|
/ RootR GET
/foo FooR GET
|]

instance Yesod Slash where
    joinPath _ ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
      where
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x
        pieces = pieces' ++ [""]

    -- Мы хотим сохранять канонические URL. Поэтому, если на конце URL отсутствует
    -- косая черта, делаем перенаправление. Но пустой набор компонентов пути остаётся
    -- как есть.
    cleanPath _ [] = Right []
    cleanPath _ s
        | dropWhile (not . T.null) s == [""] = -- единственно возможная пустая строка - последняя
            Right $ init s
        -- Т. к. joinPath добавит недостающую косую черту, мы просто
        -- удаляем пустые компоненты пути.
        | otherwise = Left $ filter (not . T.null) s

getRootR = defaultLayout [whamlet|
<p>
    <a href=@{RootR}>RootR
<p>
    <a href=@{FooR}>FooR
|]

getFooR = getRootR

main = warpDebug 3000 Slash
