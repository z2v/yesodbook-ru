Это приложение~--- простой блог. Оно позволяет администратору добавлять записи
в блог с помощью текстового редактора (nicedit), зарегистрированным
пользователям~--- оставлять комментарии, а также имеет полную поддержку
локализации. Это также хороший пример использования базы данных Persistent,
применения системы авторизации Yesod и шаблонов.

Хотя в целом мы рекомендуем размещать шаблоны, определения сущностей Persist и
маршрутизацию в отдельных файлах, здесь, для удобства, мы будем держать всё это
в одном файле. Единственным исключением, как вы увидите ниже, будут
локализованные сообщения.

Начнём с расширений языка. В коде сгенерированного шаблона сайта расширения
языка указаны в файле cabal, так что вам не нужно будет указывать их в ваших
файлах Haskell.

\begin{code}
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable #-}
\end{code}

Теперь импорт.

\begin{code}
import Yesod
import Yesod.Auth
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Yesod.Auth.BrowserId (authBrowserId, def)
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlPersistT, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import Data.Time (UTCTime, getCurrentTime)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Typeable (Typeable)
\end{code}%$

Сначала мы настроим наши сущности Persistent. Мы создадим наши типы данных
(через \lstinline!mkPersist!) и функцию миграции, которая будет автоматически
создавать и обновлять нашу SQL-схему. Если вы используете сервер MongoDB,
миграция будет не нужна.

\begin{code}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
\end{code}

Данные пользователей. В более сложных приложениях мы бы также хранили дату
регистрации, отображаемое имя и т.д.
\begin{code}
User
   email Text
   UniqueUser email
\end{code}
Чтобы работало кэширование из пакета \texttt{yesod-auth}, наш
тип~\lstinline{User} должен быть экземпляром класса~\lstinline{Typeable}.
\begin{code}
   deriving Typeable
\end{code}

Отдельная запись в блоге (я не пользуюсь словом <<пост>>, чтобы избежать
путаницы с методом POST отправки запроса).
\begin{code}
Entry
   title Text
   posted UTCTime
   content Html
\end{code}

И комментарий к записи.
\begin{code}
Comment
   entry EntryId
   posted UTCTime
   user UserId
   name Text
   text Textarea
|]
\end{code}

Каждый сайт имеет тип-основание. Это значение инициализируется перед запуском
приложения и доступно в течение всего времени работы. В нашем мы будем хранить
пул соединений с базой данных и менеджер HTTP-соединения. Как они будут
инициализированы~--- смотрите в конце этого файла.

\begin{code}
data Blog = Blog
   { connPool :: ConnectionPool
   , httpManager :: Manager
   }
\end{code}

Чтобы упростить локализацию и сделать её дружественной к переводчику, у нас
есть специальный формат файлов для перевода сообщений. Для каждого языка
существует отдельный файл, который именуется на основе кода языка (например,
<<en>>, <<es>>, <<de-DE>>) и размещается в одноимённом каталоге. Также мы
указываем основной языковой файл (в данном случае, <<en>>) в качестве языка по
умолчанию.

\begin{code}
mkMessage "Blog" "hs/18/messages-blog" "en"
\end{code}

Содержимое нашего файла <<en>>:

\lstinputlisting[language=]{../hs/18/messages-blog/en.msg}

А теперь настроим таблицу маршрутизации. У нас будет четыре записи: домашняя
страница, страница со списком записей (\lstinline!BlogR!), страница отдельной
записи (\lstinline!EntryR!) и подсайт аутентификации. Обратите внимание, что
\lstinline!BlogR! и \lstinline!EntryR! принимают методы GET и POST. POST
используется для добавления в блог, соответственно, новой записи и нового
комментария.

\begin{code}
mkYesod "Blog" [parseRoutes|
/               HomeR   GET
/blog           BlogR   GET   POST
/blog/#EntryId  EntryR  GET   POST
/auth           AuthR   Auth  getAuth
|]
\end{code}

Каждый тип-основание должен быть экземпляром класса типов \lstinline!Yesod!.
Именно здесь мы настраиваем различные параметры.

\begin{code}
instance Yesod Blog where
\end{code}

Корень нашего приложения. Обратите внимание, что для того, чтобы
\lstinline!BrowserID! работал правильно, это должен быть корректный URL.

\begin{code}
    approot = ApprootStatic "http://localhost:3000"
\end{code}

Наша схема авторизации. Мы хотим, чтобы соблюдались следующие правила:

\begin{itemize}
\item Только администраторы могут добавлять новую запись.
\item Только зарегистрированные пользователи могут добавлять комментарии.
\item Все остальные страницы доступны всем.
\end{itemize}

Мы сконфигурировали наши маршруты в RESTful-стиле: действия, которые могут
вносить изменения, всегда используют метод POST. В результате мы можем легко
проверить, является ли запрос запросом на запись: если вторым параметром
передано \lstinline!True!, значит это так.

Во-первых, мы будем авторизовать запросы на добавление новой записи.
\begin{code}
    isAuthorized BlogR True = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (Entity _ user)
                | isAdmin user -> return Authorized
                | otherwise    -> unauthorizedI MsgNotAnAdmin
\end{code}

Также мы будем авторизовать запросы на добавление нового комментария.
\begin{code}
    isAuthorized (EntryR _) True = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
\end{code}

Все остальные запросы авторизуем всегда.
\begin{code}
    isAuthorized _ _ = return Authorized
\end{code}

Указываем, куда должен быть перенаправлен пользователь, если он получает \lstinline!AuthenticationRequired!.

\begin{code}
    authRoute _ = Just (AuthR LoginR)
\end{code}

Здесь мы определяем внешний вид нашего сайта. Функция получает содержимое
отдельной страницы и оборачивает его в стандартный шаблон.
\begin{code}
    defaultLayout inside = do
\end{code}

Yesod поощряет подход <<get-following-post>>, когда после POST-запроса
пользователь перенаправляется на другую страницу. Чтобы позволить POST-странице
дать пользователю какой-то отклик, у нас есть функции \lstinline!getMessage! и
\lstinline!setMessage!. Хорошей идеей будет всегда проверять на наличие
ожидающих сообщений в вашей функции \lstinline!defaultLayout!.
\begin{code}
        mmsg <- getMessage
\end{code}

Чтобы объединить вместе HTML, CSS и JavaScript, мы используем виджеты. В конце
концов мы должны развернуть всё это в обычный HTML. Для этого есть функция
\lstinline!widgetToPageContent!. Мы передадим ей виджет, состоящий из
содержимого, которое мы получили от отдельной страницы (\lstinline!inside!), и
стандартного CSS для всех страниц. Для создания последнего мы используем язык
шаблонов Lucius.
\begin{code}
        pc <- widgetToPageContent $ do
            toWidget [lucius|
body {
    width: 760px;
    margin: 1em auto;
    font-family: sans-serif;
}
textarea {
    width: 400px;
    height: 200px;
}
#message {
  color: #900;
}
|]
            inside
\end{code}%$

В заключение мы используем новый шаблон Hamlet, чтобы обернуть отдельные части
(заголовок, содержимое тегов \lstinline!<head>! и \lstinline!<body>!) в
конечный результат.
\begin{code}
        giveUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <div #message>#{msg}
        ^{pageBody pc}
|]
\end{code}

Это простая функция для проверки, является ли пользователь администратором. В
реальных приложениях мы, скорее всего, будем хранить признак администратора в
базе данных или проверять через какую-то внешнюю систему. А сейчас я просто
жёстко задам адрес своей электронной почты.
\begin{code}
isAdmin :: User -> Bool
isAdmin user = userEmail user == "michael@snoyman.com"
\end{code}

Чтобы получить доступ к базе данных, нам нужно создать экземпляр
\lstinline!YesodPersist!, который указывает, какой сервер мы используем и как
выполнять действия.
\begin{code}
instance YesodPersist Blog where
   type YesodPersistBackend Blog = SqlPersistT
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
\end{code}

Это синоним типа для удобства. Он определяется автоматически при генерации
шаблона сайта.
\begin{code}
type Form x = Html -> MForm Handler (FormResult x, Widget)
\end{code}

Для использования yesod-form и yesod-auth нам нужен экземпляр
\lstinline!RenderMessage! для \lstinline!FormMessage!. Это позволит
контролировать локализацию отдельных сообщений форм.
\begin{code}
instance RenderMessage Blog FormMessage where
    renderMessage _ _ = defaultFormMessage
\end{code}

Этот экземпляр нужен для того, чтобы использовать встроенный HTML-редактор Nic.
Используем значения по умолчанию для CDN-hosted версии Nic.
\begin{code}
instance YesodNic Blog
\end{code}

Чтобы использовать yesod-auth, нам нужен экземпляр \lstinline!YesodAuth!.
\begin{code}
instance YesodAuth Blog where
    type AuthId Blog = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authHttpManager = httpManager
\end{code}

Мы будем использовать систему \footnotehref{https://browserid.org/}{BrowserID}
(также известную как Mozilla Persona),
использующую адрес электронной почты в качестве вашего идентификатора. Это
позволит в будущем легко перейти на другие системы для локально
аутентифицируемых адресов электронной почты (также входит в yesod-auth).
\begin{code}
    authPlugins _ = [authBrowserId def]
\end{code}

Эта функция принимает регистрационные данные пользователя (то есть, адрес
электронной почты) и возвращает \lstinline!UserId!.
\begin{code}
    getAuthId creds = do
        let email = credsIdent creds
            user = User email
        res <- runDB $ insertBy user
        return $ Just $ either entityKey id res
\end{code}%$

Обработчик домашней страницы. Одна важная деталь здесь~--- это использование
\lstinline'`setTitleI`', что позволяет использовать локализованные сообщения в
заголовке страницы. Мы также используем это сообщение с
\lstinline'`_{Msg...}`'-интерполяцией в Hamlet.
\begin{code}
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitleI MsgHomepageTitle
    [whamlet|
<p>_{MsgWelcomeHomepage}
<p>
   <a href=@{BlogR}>_{MsgSeeArchive}
|]
\end{code}%$

Определяем форму для добавления новых записей. Мы хотим, чтобы пользователь
заполнил заголовок и содержание, а дату создания записи заполним автоматически
с помощью \lstinline'`getCurrentTime`'.

Обратите внимание на странноватую манеру выполнения действий ввода/вывода:
\lstinline!lift (liftIO getCurrentTime)!. Причина: аппликативные формы не
являются монадами и поэтому не могут быть экземплярами класса~\lstinline{MonadIO}.
Вместо этого, мы используем \lstinline!lift!, чтобы выполнить действие в нижележащей
монаде~\lstinline{Handler}, и~\lstinline{liftIO}, чтобы конвертировать действие
ввода/вывода в действие~\lstinline{Handler}.

\begin{code}
entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing
\end{code}

Получаем список всех записей и для администратора отображаем форму создания
новой записи.
\begin{code}
getBlogR :: Handler Html
getBlogR = do
    muser <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        [whamlet|
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall Entity entryId entry <- entries
            <li>
                <a href=@{EntryR entryId}>#{entryTitle entry}
\end{code}%$

У нас есть три варианта: пользователь-администратор вошёл в систему, обычный
пользователь вошёл в систему и пользователь не вошёл в систему. В первом случае
мы должны отобразить форму создания записи. Во втором мы ничего не делаем. В
третьем мы отображаем ссылку для входа.
\begin{code}
$maybe Entity _ user <- muser
    $if isAdmin user
        <form method=post enctype=#{enctype}>
            ^{entryWidget}
            <div>
                <input type=submit value=_{MsgNewEntry}>
$nothing
    <p>
        <a href=@{AuthR LoginR}>_{MsgLoginToPost}
|]
\end{code}%$

Обрабатываем добавление новой записи. Мы не делаем никакой проверки
авторизации, так как \lstinline!isAuthorized! делает это за нас. Если в форме
заданы корректные значения, мы добавляем запись в базу данных и перенаправляем
пользователя на эту запись. В противном случае мы просим пользователя
попробовать ещё раз.
\begin{code}
postBlogR :: Handler Html
postBlogR = do
    ((res, entryWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessageI $ MsgEntryCreated $ entryTitle entry
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectEntry
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=_{MsgNewEntry}>
|]
\end{code}%$

Форма для комментариев, очень похожая на \lstinline!entryForm! выше. Она
принимает \lstinline!EntryID! записи, к которой относится комментарий.
Используя \lstinline!pure!, мы вкладываем это значение в результирующее
значение \lstinline!Comment!, не позволяя ему появиться в сгенерированном HTML.
\begin{code}
commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
\end{code}

Показываем отдельную запись, комментарии и, для зарегистрированных
пользователей, форму добавления комментария.
\begin{code}
getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    (entry, comments) <- runDB $ do
        entry <- get404 entryId
        comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
        return (entry, map entityVal comments)
    muser <- maybeAuth
    (commentWidget, enctype) <-
        generateFormPost (commentForm entryId)
    defaultLayout $ do
        setTitleI $ MsgEntryTitle $ entryTitle entry
        [whamlet|
<h1>#{entryTitle entry}
<article>#{entryContent entry}
    <section .comments>
        <h1>_{MsgCommentsHeading}
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted _user name text <- comments
                <div .comment>
                    <span .by>#{name}
                    <span .at>#{show posted}
                    <div .content>#{text}
        <section>
            <h1>_{MsgAddCommentHeading}
            $maybe _ <- muser
                <form method=post enctype=#{enctype}>
                    ^{commentWidget}
                    <div>
                        <input type=submit value=_{MsgAddCommentButton}>
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>_{MsgLoginToComment}
|]
\end{code}%$

Получаем добавляемый комментарий.
\begin{code}
postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    ((res, commentWidget), enctype) <-
        runFormPost (commentForm entryId)
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessageI MsgCommentAdded
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectComment
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{commentWidget}
    <div>
        <input type=submit value=_{MsgAddCommentButton}>
|]
\end{code}%$

Наконец, наша главная функция.
\begin{code}
main :: IO ()
main = do
    -- создаём новый пул
    pool <- createSqlitePool "blog.db3" 10
    -- выполняем необходимую миграцию
    runSqlPersistMPool (runMigration migrateAll) pool
    -- создаём новый менеджер HTTP
    manager <- newManager defaultManagerSettings
    -- запускаем наш сервер
    warp 3000 $ Blog pool manager
\end{code}
