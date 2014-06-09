\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Chat
import           Control.Concurrent.Chan (newChan)
import qualified Data.IORef              as I
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as TL
import           Text.Markdown           (def, markdown)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy        (authDummy)
\end{code}
}

\subsection{Данные}
Теперь мы можем перейти к написанию основного приложения. Это приложение будет
включать подсайт чата и вики. Первое, с чем мы должны определиться,~--- как
хранить содержимое вики. Обычно для этого используют какое-нибудь постоянное
хранилище данных. Мы для простоты будем всё хранить в памяти. Каждая страница
вики идентифицируется списком имён, для содержимого ограничимся текстом. В итоге,
наш тип-основание примет вид:
\begin{code}
data App = App
    { getChat     :: Chat
    , wikiContent :: I.IORef (Map.Map [Text] Text)
    }
\end{code}

Теперь опишем наши маршруты:
\begin{code}
mkYesod "App" [parseRoutes|
/            HomeR GET             -- домашняя страница
/wiki/*Texts WikiR GET POST        -- обратите внимание на компонент для иерархии вики

/chat        ChatR Chat getChat    -- подсайт чата
/auth        AuthR Auth getAuth    -- подсайт авторизации
|]
\end{code}

\subsection{Экземпляры классов типов} Нам потребуется внести два изменения в
экземпляр по умолчанию для класса~\lstinline'Yesod'.  Во-первых, мы хотим
добавить реализацию функции~\lstinline'authRoute', чтобы наш чат мог корректно
отобразить ссылку для входа на сайт. Во-вторых, мы переопределим
функцию~\lstinline'defaultLayout'.  Помимо ссылок для входа на сайт и выхода с
него, наша функция будет добавлять виджет чата на каждую страницу.
\begin{code}
instance Yesod App where
    authRoute _ = Just $ AuthR LoginR -- получаем рабочую ссылку для входа

    -- Наша функция defaultLayout будет добавлять виджет чата на каждую
    -- страницу. Также вверху страницы добавляем ссылки для входа/выхода
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
            chatWidget ChatR
        mmsg <- getMessage
        giveUrlRenderer
            [hamlet|
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
\end{code}

Так как мы используем подсайт чата, мы должны определить экземпляр
класса~\lstinline'YesodChat'.
\begin{code}
instance YesodChat App where
    getUserName = do
        muid <- maybeAuthId
        case muid of
            Nothing -> do
                setMessage "Не вошли на сайт"
                redirect $ AuthR LoginR
            Just uid -> return uid
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma
\end{code}

Экземпляры классов~\lstinline'YesodAuth' и~\lstinline'RenderMessage', так же
как и обработчик домашней страницы, в целом очевидны:
\begin{code}
-- Относительно стандартный экземпляр YesodAuth. Будем использовать фиктивный
-- плагин авторизации, так что вы сможете использовать любое имя по желанию,
-- которое будет сохранено как AuthId.
instance YesodAuth App where
    type AuthId App = Text
    authPlugins _ = [authDummy]
    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authHttpManager = error "authHttpManager" -- authDummy не использует
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Ничего особенного, просто даём ссылку на корень вики
getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Добро пожаловать на Вики!
        <p>
            <a href=@{wikiRoot}>Начало
    |]
  where
    wikiRoot = WikiR []
\end{code}

\subsection{Обработчики вики}
Теперь самое время написать обработчики запросов для вики: GET для показа
страницы и POST для обновления. Также определим
функцию~\lstinline'wikiForm' для использования в обоих обработчиках.
\begin{code}
-- Форма для получения содержимого вики
wikiForm :: Maybe Textarea -> Html -> MForm Handler (FormResult Textarea, Widget)
wikiForm mtext = renderDivs $ areq textareaField "Текст страницы" mtext

-- Показываем страницу вики и форму для редактирования
getWikiR :: [Text] -> Handler Html
getWikiR page = do
    -- Получаем ссылку на хранилище содержимого
    icontent <- fmap wikiContent getYesod

    -- Читаем содержимое по ссылке And read the map from inside the reference
    content <- liftIO $ I.readIORef icontent

    -- Ищем содержимое текущей страницы, если имеется
    let mtext = Map.lookup page content

    -- Создаём форму с текущим содержимым в виде исходного значения
    -- Заметьте, мы используем обёртку Textarea для получения тега <textarea>
    (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- Считаем, что ввод в формате markdown. В пакете markdown
            -- реализован механизм защиты от XSS
            Just text -> toWidget $ markdown def $ TL.fromStrict text
            Nothing -> [whamlet|<p>Страница ещё не существует|]
        [whamlet|
            <h2>Edit page
            <form method=post>
                ^{form}
                <div>
                    <input type=submit>
        |]

-- Получаем отправленную страницу и обновлённое содержимое
postWikiR :: [Text] -> Handler Html
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
        _ -> defaultLayout
                [whamlet|
                    <form method=post>
                        ^{form}
                        <div>
                            <input type=submit>
                |]
\end{code}

\subsection{Запуск}

Наконец, мы готовы к запуску нашего приложения. В отличие от большинства
предыдущих примеров в этой книги, теперь нам надо выполнить настоящую
инициализацию в функции~\lstinline'main'. Для подсайта~\lstinline'Chat'
требуется созданный пустой канал~\lstinline'Chan', и ещё требуется изменяемая
переменная (mutable variable) для хранения содержимого вики. Как только они у
нас есть, мы создаём значение типа~\lstinline'App' и передаём его
функции~\lstinline'warp'.
\begin{code}
main :: IO ()
main = do
    -- Создаём канал событий сервера
    chan <- newChan

    -- Изначально пустая база данных для страниц вики
    icontent <- I.newIORef Map.empty

    -- Запуска наше приложение
    warpEnv App
        { getChat = Chat chan
        , wikiContent = icontent
        }
\end{code}
