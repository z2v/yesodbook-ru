{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts
  #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Dummy (authDummy)
import Chat
import Control.Concurrent.Chan (Chan, newChan)
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.IORef as I
import qualified Data.Map as Map

-- Этот пакет пока отсутствует на Hackage.
-- Вы можете скачать его исходный код с https://github.com/snoyberg/markdown
import Text.Markdown (markdown, def)

-- | Наш тип-основание включает и подсайт чата, и изменяемую ссылку на словарь
-- содержимого wiki. Обратите внимание, что ключом в этом словаре является список
-- из Text, так как wiki может иметь произвольную иерархию.
--
-- В реальном приложении мы бы предпочли хранить эту информацию в какой-нибудь
-- базе данных.
data Wiki = Wiki
    { getChat :: Chat
    , wikiContent :: I.IORef (Map.Map [Text] Text)
    }

-- Настраиваем наши маршруты, как обычно.
mkYesod "Wiki" [parseRoutes|
/ RootR GET                 -- домашняя страница
FIXME
/wiki/*Texts WikiR GET POST -- обратите внимание на многокомпонентный путь,
                            -- необходимый для иерархии wiki
/chat ChatR Chat getChat    -- подсайт чата
/auth AuthR Auth getAuth    -- подсайт аутентификации
|]

instance Yesod Wiki where
    -- получим работающую ссылку на вход
    authRoute _ = Just $ AuthR LoginR

    -- Наш особый defaultLayout добавит виджет чата на каждую страницу.
    -- Он также добавит ссылки на вход и на выход вверху страниц.
    defaultLayout widget = do
        pc <- widgetToPageContent $ widget >> chatWidget ChatR
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <div .message>#{msg}
        <nav>
            <a href=@{AuthR LoginR}>Войти
            \ | #
            <a href=@{AuthR LogoutR}>Выйти
        ^{pageBody pc}
|]

-- Довольно стандартный экземпляр YesodAuth. Мы будем использовать плагин-пустышку,
-- чтобы вы могли создать любое имя, какое пожелаете, и хранить имя вошедшего
-- пользователя как AuthId.
instance YesodAuth Wiki where
    type AuthId Wiki = Text
    authPlugins _ = [authDummy]
    loginDest _ = RootR
    logoutDest _ = RootR
    getAuthId = return . Just . credsIdent
    authHttpManager = error "authHttpManager" -- authDummy это не использует

-- Просто реализуем аутентификацию на основе нашего yesod-auth.
instance YesodChat Wiki where
    getUserName = requireAuthId
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma

instance RenderMessage Wiki FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Здесь ничего особенного, просто даём ссылку на корень wiki.
getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
<p>Добро пожаловать в Wiki!
<p>
    <a href=@{wikiRoot}>Корень wiki
|]
  where
    wikiRoot = WikiR []

-- Форма для получения wiki-содержимого
wikiForm mtext = renderDivs $ areq textareaField "Содержимое страницы" mtext

-- Отобразить страницу wiki и форму для редактирования
getWikiR :: [Text] -> Handler RepHtml
getWikiR page = do
    -- Получим ссылку на словарь содержимого
    icontent <- fmap wikiContent getYesod

    -- И прочтём словарь из ссылки
    content <- liftIO $ I.readIORef icontent

    -- Получим содержимое текущей страницы, если оно доступно
    let mtext = Map.lookup page content

    -- Сгенерируем форму с текущим содержимым в качестве значения по умолчанию.
    -- Обратите внимание, что мы используем обёртку Textarea для получения <textarea>.
    (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- Трактуем ввод как markdown. Пакет markdown автоматически
            -- выполняет для нас защиту от XSS.
            Just text -> toWidget $ markdown def $ TL.fromStrict text
            Nothing -> [whamlet|<p>Страница не существует|]
        [whamlet|
<h2>Редактировать страницу
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]

-- Получим отправленную wiki-страницу и обновим содержимое.
postWikiR :: [Text] -> Handler RepHtml
postWikiR page = do
    icontent <- fmap wikiContent getYesod
    content <- liftIO $ I.readIORef icontent
    let mtext = Map.lookup page content
    ((res, form), _) <- runFormPost $ wikiForm $ fmap Textarea mtext
    case res of
        FormSuccess (Textarea t) -> do
            liftIO $ I.atomicModifyIORef icontent $
                \m -> (Map.insert page t m, ())
            setMessage "Страница обновлена"
            redirect $ WikiR page
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]

main :: IO ()
main = do
    -- Создадим наш канал серверных событий
    chan <- newChan

    -- Изначально у нас пустая база данных wiki-страниц
    icontent <- I.newIORef Map.empty

    -- Запустим наше приложение
    warpDebug 3000 $ Wiki (Chat chan) icontent
