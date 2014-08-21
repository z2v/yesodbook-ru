{-# LANGUAGE QuasiQuotes #-}
import Text.Lucius
import qualified Data.Text.Lazy.IO as TLIO

-- Заглушка для функции рендеринга
render = undefined

-- Наша примесь, предоставляющая ряд префиксов вендоров для переходов
transition val =
    [luciusMixin|
        -webkit-transition: #{val};
        -moz-transition: #{val};
        -ms-transition: #{val};
        -o-transition: #{val};
        transition: #{val};
    |]

-- Шаблон Lucius, который использует примесь
myCSS =
    [lucius|
        .some-class {
            ^{transition "all 4s ease"}
        }
    |]

main = TLIO.putStrLn $ renderCss $ myCSS render
