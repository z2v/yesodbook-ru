{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
import Yesod
import Yesod.Form.Jquery
import Data.Time (Day)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))

data Synopsis = Synopsis

mkYesod "Synopsis" [parseRoutes|
/ RootR GET
/person PersonR POST
|]

instance Yesod Synopsis

-- Указывает приложению использовать стандартные английские сообщения
-- Если вам нужна i18n, вы можете предоставить функцию перевода
instance RenderMessage Synopsis FormMessage where
    renderMessage _ _ = defaultFormMessage

-- И указывает, где найти библиотеки jQuery. Мы будем использовать значение
-- по умолчанию, указывающее на Google CDN
instance YesodJquery Synopsis

-- Тип данных, который мы хотим получить из формы
data Person = Person
    { personName :: Text
    , personBirthday :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail :: Text
    , personWebsite :: Maybe Text
    }
  deriving Show

-- Объявление формы. Сигнатура типа несколько пугающая, но вот её обзор:
--
-- * Параметр Html используется для кодирования некоторой дополнительной
-- информации. См. обсуждение runFormGet и runFormPost ниже для
-- дополнительного объяснения
--
-- * Как обычно, у нас есть типы под- и главного сайта (FIXME: ХРЕНЬ)
--
-- * FormResult может находиться в трех состояниях: FormMissing (нет
-- доступных данных), FormFailure (некорректные данные) и FormSuccess
--
-- * Widget --- отображаемая форма для вставки на страницу
--
-- Обратите внимание, что шаблон сайта предоставляет удобный синоним типа
-- Form, так что наша сигнатура может быть переписана как:
-- > personForm :: Form Person
--
-- Для целей обучения лучше видеть полную версию
personForm :: Html -> MForm Synopsis Synopsis (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Имя" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- выпадающий список
        , jdsYearRange = "1900:-5" -- от 1900 года до пятилетней давности
        }) "Дата рождения" Nothing
    <*> aopt textField "Любимый цвет" Nothing
    <*> areq emailField "Адрес Email" Nothing
    <*> aopt urlField "Сайт" Nothing

-- Обработчик GET-запроса отображает форму
getRootR :: Handler RepHtml
getRootR = do
    -- генерируем форму, которую будем отображать
    (widget, enctype) <- generateFormPost personForm
    defaultLayout [whamlet|
<p>Сгенерированный виджет включает только содержимое формы, без тега form.
Поэтому...
<form method=post action=@{PersonR} enctype=#{enctype}>
    ^{widget}
    <p>В нём также нет кнопки отправки формы.
    <input type=submit>
|]

-- Обработчик POST-запроса обрабатывает форму. Если обработка успешно
-- завершилась, он отображает данные переданного человека. Иначе -- снова
-- форму с сообщениями об ошибке.
postPersonR :: Handler RepHtml
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout [whamlet|
<p>Некорректный ввод, попробуйте ещё раз.
<form method=post action=@{PersonR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

main :: IO ()
main = warpDebug 3000 Synopsis
