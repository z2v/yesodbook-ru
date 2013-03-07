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

renderEnglish :: Msg -> Text
renderEnglish Hello = "Привет"
renderEnglish (Apples 0) = "Вы не купили яблок."
renderEnglish (Apples 1) = "Вы купили одно яблоко."
renderEnglish (Apples i) = T.concat ["Вы купили ", T.pack $ show i, " яблок."]

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
     $ (template 5) (toHtml . renderEnglish) renderUrl
