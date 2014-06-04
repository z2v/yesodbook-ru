\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
--- | Этот модуль определяет подсайт, который позволяет вам вставлять
--- элемент управления с чатом в любую страницу вашего сайта. Он использует
--- eventsource для отправки сообщений браузеру с сервера.
module Chat
    ( module Chat
    , module Chat.Data
    ) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Chat.Data
import           Control.Concurrent.Chan            (dupChan, writeChan)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Network.Wai.EventSource            (ServerEvent (..),
                                                     eventSourceAppChan)
import           Yesod

\end{code}
}

\subsection{Обработчики}
Теперь, когда мы определили тип-основание и маршруты, мы должны создать отдельный
модуль, реализующий функционал подсайта. Мы назовём этот модуль~\texttt{Chat},
и именно с него мы начнём рассматривать функционирование подсайта.

Подсайт представляет собой слой поверх некоторого основного сайта, который
будет предоставлен пользователем. В большинстве случае, подсайту потребуется
специфическая информация от основного сайта. В случае нашего чата, мы хотим,
чтобы основной сайт предоставлял механизм аутентификации пользователей.
Подсайту требуется возможность определить, является ли текущий пользователь
аутентифицированным на сайте, и получить его имя.

Наш способ реализации этой концепции~--- определение класса типов, который
инкапсулирует необходимую функциональность. Давайте посмотрим на наш класс
типов~\lstinline!YesodChat!:
\begin{code}
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: HandlerT master IO Text
    isLoggedIn :: HandlerT master IO Bool
\end{code}

Любой сайт, который захочет использовать наш чат, должен предоставлять
экземпляр класса~\lstinline'YesodChat'. (Ниже увидим, как сделать это
требование обязательным.) Тут есть пара интересных моментов:
\begin{itemize}
    \item Мы можем добавить ещё ограничений для нашего основного сайта,
    например: предоставлять экземпляр класса \lstinline'Yesod' или разрешить
    рендеринг сообщений форм.  Первое позволит нам использовать
    \lstinline'defaultLayout', а второе~--- стандартные виджеты форм.

    \item Выше по тексту, мы частенько использовали монаду~\lstinline'Handler'.
    Помните, что~\lstinline'Handler'~--- это специфический для каждого приложения
    синоним типа~\lstinline'HandlerT'. Так как предполагается, что код
    будет работать с множеством различных приложений, мы используем полную
    форму трансформатора~\lstinline'HandlerT'.
\end{itemize}

Speaking of the +Handler+ type synonym, we're going to want to have something
similar for our subsite. The question is: what does this monad look like? In a
subsite situation, we end up with two layers of +HandlerT+ transformers: one
for the subsite, and one for the master site. We want to have a synonym that
works for any master site which is an instance of +YesodChat+, so we end up
with:

\begin{code}
type ChatHandler a =
    forall master . YesodChat master =>
    HandlerT Chat (HandlerT master IO) a
\end{code}

Now that we have our machinery out of the way, it's time to write our subsite
handler functions. We had two routes: one for sending messages, and one for
receiving messages. Let's start with sending. We need to:

. Get the username for the person sending the message.
. Parse the message from the incoming parameters. (Note that we're going to use GET parameters for simplicity of the client-side Ajax code.)
. Write the message to the +Chan+.

The trickiest bit of all this code is to know when to use +lift+. Let's look at
the implementation, and then discuss those +lift+ usages:

\begin{code}
postSendR :: ChatHandler ()
postSendR = do
    from <- lift getUserName
    body <- lift $ runInputGet $ ireq textField "message"
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body
\end{code}

+getUserName+ is the function we defined in our +YesodChat+ typeclass earlier.
If we look at that type signature, we see that it lives in the master site's
+Handler+ monad. Therefore, we need to +lift+ that call out of the subsite.

The call to +runInputGet+ is a little more subtle. Theoretically, we could run
this in either the subsite or the master site. However, we use +lift+ here as
well for one specific reason: message translations. By using the master site,
we can take advantage of whatever +RenderMessage+ instance the master site
defines. This also explains why we have a +RenderMessage+ constraint on the
+YesodChat+ typeclass.

The next call to +getYesod+ is \emph{not} \texttt{lift}ed. The reasoning here is simple:
we want to get the subsite's foundation type in order to access the message
channel. If we instead \texttt{lift}ed that call, we'd get the master site's
foundation type instead, which is not what we want in this case.

The final line puts the new message into the channel. Since this is an +IO+
action, we use +liftIO+. +ServerEvent+ is part of the +wai-eventsource+
package, and is the means by which we're providing server-sent events in this
example.

The receiving side is similarly simple:

\begin{code}
getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    req <- waiRequest
    res <- liftIO $ eventSourceAppChan chan req
    sendWaiResponse res
\end{code}

We use +dupChan+ so that each new connection receives its own copy of newly
generated messages. This is a standard method in concurrent Haskell of creating
braodcast channels. The last three lines of our function expose the underlying
+wai-eventsource+ application as a Yesod handler, by getting the raw WAI
request, running the application on that request, and then sending the raw WAI
response.

NOTE: Starting with WAI 2.0, instead of using +liftResourceT+, you need to use
+liftIO+.

Now that we've defined our handler functions, we can set up our dispatch. In a
normal application, dispatching is handled by calling +mkYesod+, which creates
the appropriate +YesodDispatch+ instance. In subsites, things are a little bit
more complicated, since you'll often want to place constraints on the master
site. The formula we use is the following:

\begin{code}
instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)
\end{code}

We're stating that our +Chat+ subsite can live on top of any master site which
is an instance of +YesodChat+. We then use the +mkYesodSubDispatch+ Template
Haskell function to generate all of our dispatching logic. While this is a bit
more difficult to write than +mkYesod+, it provides necessary flexibility, and
is mostly identical for any subsite you'll write.

\subsection{Виджет}

We now have a fully working subsite. The final component we want as part of our
chat library is a widget to be embedded inside a page which will provide chat
functionality. By creating this as a widget, we can include all of our HTML,
CSS, and Javascript as a reusable component.

Our widget will need to take in one argument: a function to convert a +Chat+
subsite URL into a master site URL. The reasoning here is that a application
developet could place the chat subsite anywhere in the URL structure, and this
widget needs to be able to generate Javascript which will point at the correct
URLs. Let's start off our widget:

\begin{code}
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> WidgetT master IO ()
chatWidget toMaster = do
\end{code}

Next, we're going to generate some identifiers to be used by our widget. It's
always good practice to let Yesod generate unique identifiers for you instead
of creating them manually to avoid name collissions.

\begin{code}
    chat <- newIdent   -- обрамляющий div
    output <- newIdent -- элемент, где отображаются сообщения
    input <- newIdent  -- поле пользовательского ввода
\end{code}

And next we need to check if the user is logged in, using the +isLoggedIn+
function in our +YesodChat+ typeclass. Since we're in a +Widget+ and that
function lives in the +Handler+ monad, we need to use +handlerToWidget+:

\begin{code}
    ili <- handlerToWidget isLoggedIn  -- проверить, выполнили ли мы вход
\end{code}

If the user is logged in, we want to display the chat box, style it with some
CSS, and then make it interactive using some Javascript. This is mostly
client-side code wrapped in a Widget:

\begin{code}
    if ili
        then do
            -- Вход выполнен: показать виджет
            [whamlet|
                <div ##{chat}>
                    <h2>Chat
                    <div ##{output}>
                    <input ##{input} type=text placeholder="Введите сообщение">
            |]
            -- Немного CSS
            toWidget [lucius|
                ##{chat} {
                    position: absolute;
                    top: 2em;
                    right: 2em;
                }
                ##{output} {
                    width: 200px;
                    height: 300px;
                    border: 1px solid #999;
                    overflow: auto;
                }
            |]
            -- И теперь вот такой Javascript
            toWidgetBody [julius|
                // Настроим принимающую сторону
                var output = document.getElementById(#{toJSON output});
                var src = new EventSource("@{toMaster ReceiveR}");
                src.onmessage = function(msg) {
                    // Эта функция будет вызвана для каждого нового сообщения.
                    var p = document.createElement("p");
                    p.appendChild(document.createTextNode(msg.data));
                    output.appendChild(p);
                
                    // А теперь прокрутим вниз в обрамляющем div так, чтобы было видно
                    // последнее сообщение.
                    output.scrollTop = output.scrollHeight;
                };
                
                // Настроим отправляющую сторону: отправлять сообщение через Ajax всякий раз,
                // когда пользователь нажимает Enter.
                var input = document.getElementById(#{toJSON input});
                input.onkeyup = function(event) {
                    var keycode = (event.keyCode ? event.keyCode : event.which);
                    if (keycode == '13') {
                        var xhr = new XMLHttpRequest();
                        var val = input.value;
                        input.value = "";
                        var params = "?message=" + encodeURI(val);
                        xhr.open("POST", "@{toMaster SendR}" + params);
                        xhr.send(null);
                    }
                }
            |]
\end{code}

And finally, if the user isn't logged in, we'll ask them to log in to use the
chat app.

\begin{code}
        else do
            -- Пользователь не выполнил вход, выдать об этом сообщение.
            master <- getYesod
            [whamlet|
                <p>
                    Вы должны #
                    $maybe ar <- authRoute master
                        <a href=@{ar}>выполнить вход,
                    $nothing
                        выполнить вход,
                    \ чтобы воспользоваться чатом.
            |]
\end{code}
