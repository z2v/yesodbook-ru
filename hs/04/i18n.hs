{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (HtmlUrlI18n, ihamlet)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl Home _ = "/home"
renderUrl Time _ = "/time"
renderUrl Stylesheet _ = "/style.css"

data Msg = Hello | Apples Int

renderRussian :: Msg -> Text
renderRussian Hello = "Привет"
renderRussian (Apples 0) = "Вы не купили яблок."
renderRussian (Apples 1) = "Вы купили одно яблоко."
renderRussian (Apples i) = T.concat ["Вы купили ", T.pack $ show i, if i < 5 then " яблока." else " яблок."]

template :: Int -> HtmlUrlI18n Msg MyRoute
template count = [ihamlet|
                 $doctype 5
                 <html>
                     <head>
                             <title>i18n
                                 <body>
                                         <h1>_{Hello}
                                                 <p>_{Apples count}
                                                 |]

main :: IO ()
main = putStrLn $ renderHtml
     $ (template 5) (toHtml . renderRussian) renderUrl
