-- Пока что игнорируйте QuasiQuotes и функцию shamlet, мы всë это объясним позже
{-# LANGUAGE QuasiQuotes #-}

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)

data Person = Person
    { name :: String
    , age  :: Int
    }

main :: IO ()
main = putStrLn $ renderHtml [shamlet|
<p>Привет, меня зовут #{name person} и мне #{show $ age person}.
<p>
    Давайте сделаем с моим именем разные смешные штуки: #
    <b>#{sort $ map toLower (name person)}
<p>Ох, а через 5 лет мне будет #{show ((+) 5 (age person))}.
|]
  where
    person = Person "Michael" 26
