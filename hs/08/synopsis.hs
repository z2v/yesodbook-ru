{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Form.Jquery

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/person PersonR POST
|]

instance Yesod App

-- Указывает приложению использовать стандартные английские сообщения
-- Если вам нужна интернационализация, вы можете задать функцию перевода
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Укажите также, где найти библиотеки jQuery. Мы будем использовать
-- значение по умолчанию, указывающее на Google CDN
instance YesodJquery App

-- Тип данных, который мы хотим получить из формы
data Person = Person
    { personName          :: Text
    , personBirthday      :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail         :: Text
    , personWebsite       :: Maybe Text
    }
  deriving Show

-- Объявление формы. Сигнатура типа несколько пугающая, но вот её обзор:
--
-- * Параметр Html используется для кодирования некоторой дополнительной
-- информации. См. обсуждение runFormGet и runFormPost ниже для
-- дополнительного объяснения
--
-- * Мы используем нашу монаду Handler, как внутренню монаду, которая
-- указывает, в каком сайте выполняется код
--
-- * FormResult может находиться в трёх состояниях: FormMissing (нет
-- доступных данных), FormFailure (некорректные данные) и FormSuccess
--
-- * Widget --- отображаемая форма для вставки на страницу
--
-- Обратите внимание, что каркас сайта предоставляет удобный синоним типа
-- Form, так что наша сигнатура может быть переписана как:
--
-- > personForm :: Form Person
--
-- Для целей обучения лучше видеть полную версию
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Имя" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- выпадающий список с выбором года
        , jdsYearRange = "1900:-5" -- от 1900 года до пятилетней давности
        }) "Дата рождения" Nothing
    <*> aopt textField "Любимый цвет" Nothing
    <*> areq emailField "Адрес email" Nothing
    <*> aopt urlField "Сайт" Nothing


-- Обработчик GET-запроса отображает форму
getHomeR :: Handler Html
getHomeR = do
    -- Генерируем форму, которую будем отображать
    (widget, enctype) <- generateFormPost personForm
    defaultLayout
        [whamlet|
            <p>
                Сгенерированный виджет включает только содержимое формы,
                без тега form. Поэтому...
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}
                <p>В виджете также нет кнопки отправки формы.
                <button>Отправить
        |]

-- Обработчик POST-запроса обрабатывает форму. Если обработка успешно
-- завершилась, он отображает данные переданного человека. В ином случае --
-- снова форму с сообщениями об ошибке.
postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <p>Некорректный ввод, попробуйте ещё раз.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Отправить
            |]

main :: IO ()
main = warp 3000 App
