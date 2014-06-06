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
    \item Мы добавили ещё ограничений для нашего основного сайта:
    предоставлять экземпляр класса \lstinline'Yesod' и разрешить
    рендеринг сообщений форм.  Первое позволит нам использовать
    \lstinline'defaultLayout', а второе~--- стандартные виджеты форм.

    \item Выше по тексту, мы частенько использовали монаду~\lstinline'Handler'.
    Помните, что~\lstinline'Handler'~--- это специфический для каждого приложения
    синоним типа~\lstinline'HandlerT'. Так как предполагается, что код
    будет работать с множеством различных приложений, мы используем полную
    форму трансформатора~\lstinline'HandlerT'.
\end{itemize}

Кстати о синониме типа~\lstinline'Handler', нам хотелось бы получить что-нибудь
подобное для нашего подсайта. Вопрос: что же из себя представляет такая монада?
В случае подсайта, мы получаем два слоя трансформаторов~\lstinline'HandlerT':
один для подсайта, другой для основного сайта. Мы хотим получить синоним,
который работает для любого основного сайта, являющегося экземпляром класса
типов~\lstinline'YesodChat', что приводит к следующему:
\begin{code}
type ChatHandler a =
    forall master . YesodChat master =>
    HandlerT Chat (HandlerT master IO) a
\end{code}

Теперь, когда структура готова, пришло время написать обработчики для нашего
подсайта.  У нас есть два маршрута: один для отправки сообщений, другой для их
получения. Начнём с отправки. Нам требуется:
\begin{itemize}
    \item Получить имя пользователя отправителя сообщения.
    \item Выбрать сообщение из входящих параметров. (Мы собираемся использовать
    параметры GET запроса для упрощения клиентского кода Ajax.)
    \item Записать сообщение в канал~\lstinline'Chan'.
\end{itemize}

Самое главная хитрость в этом коде~--- знать, когда использовать
функцию~\lstinline'lift'.  Давайте посмотрим на реализацию, а затем обсудим все
использования функции~\lstinline'lift'.

\begin{code}
postSendR :: ChatHandler ()
postSendR = do
    from <- lift getUserName
    body <- lift $ runInputGet $ ireq textField "message"
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body
\end{code}

\lstinline'getUserName'~--- эта функция, которую мы определили выше в нашем
классе типов~\lstinline'YesodChat'. Если мы посмотрим на её сигнатуру, то увидим, что она
живёт в монаде~\lstinline'Handler' основного сайта. Следовательно, нам нужно
использовать \lstinline'lift', чтобы <<поднять>> её из подсайта.

С вызовом функции~\lstinline'runInputGet' дело обстоит несколько тоньше.
Теоретически, мы могли бы выполнять эту функция как в монаде подсайта, так и в
монаде основного сайта.  Но всё же мы используем здесь функцию~\lstinline'lift'
по одной специфической причине: переводы сообщений. Используя монаду основного
сайта, мы получаем возможность пользоваться определением экземпляра класса
типов~\lstinline'RenderMessage' для основного сайта. Это также объясняет, зачем
нам ограничение \lstinline'RenderMessage' в описании класса
типов~\lstinline'YesodChat'.

Следующий далее вызов функции~\lstinline'getYesod' \emph{не}
содержит~\lstinline'lift'.  Объяснение простое: мы хотим получить тип-основание
нашего подсайта и через него доступ к каналу сообщений. Если мы добавим
\lstinline'lift', то получим тип-основание для основного сайта. А это не то,
что мы хотим в данном случае.

Последняя строка помещает сообщение в канал. Так как это действие ввода/вывода,
мы используем~\lstinline'liftIO'. Тип~\lstinline'ServerEvent' входит в состав
пакета~\texttt{wai-eventsource} и является тем средством, с помощью которого
мы реализуем отправляемые сервером сообщения в нашем примере.

Получающая сторона столь же проста:
\begin{code}
getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    req <- waiRequest
    res <- liftIO $ eventSourceAppChan chan req
    sendWaiResponse res
\end{code}

Мы используем функцию~\lstinline'dupChan', чтобы каждое соединение получало
собственную копию вновь созданных сообщений. Это стандартный способ создания
широковещательных каналов в конкурентных (concurrent) программах на~Haskell.
Последние три строки нашей функции выставляет низкоуровневое приложение из
пакета~\texttt{wai-eventsource} как обработчик Yesod: получаем необработанный
запрос WAI, выполняем приложение с этим запросом и затем отправляем такой же
необработанный ответ~WAI.

% [Vladimir Zakharov] Код соответствует WAI > 2.0, поэтому инвертировал примечание.
\begin{remark}
    При использовании версии WAI < 2.0, в предпоследней строке необходимо
    заменить вызов~\lstinline'liftIO' на вызов~\lstinline'liftResourceT'.
\end{remark}

Теперь, когда мы определили наши функции-обработчики, мы можем заняться
диспетчеризацией. В обычном приложении, диспетчеризация обеспечивается вызовом
функции~\lstinline'mkYesod', которая создаёт соответствующий экземпляр
класса~\lstinline'YesodDispatch'. Для подсайтов всё немного сложнее, так как
часто вам захочется наложить какие-нибудь ограничения на основной сайт.
Поэтому мы используем следующий подход:
\begin{code}
instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)
\end{code}

Мы постулируем, что наш подсайт~\lstinline'Chat' может работать с любым
основным сайтом, который реализует экземпляр класса~\lstinline'YesodChat'.
Затем мы используем функцию~\lstinline'mkYesodSubDispatch' расширения Template
Haskell для генерации всей логики диспетчеризации. Хотя это и немного сложнее,
чем вызов~\lstinline'mkYesod', зато этот код предоставляет требуемый уровень
гибкости и будет практически идентичным для всех подсайтов, которые вы станете
делать.

\subsection{Виджет}
Теперь у нас есть полностью рабочий подсайт чата. Последний элемент, который
мы хотим добавить в нашу библиотеку чата,~--- это виджет, предоставляющий
функциональность чата, для встраивания в страницы. Оформив этот код в виде
виджета, мы сможем включать весь требуемый код HTML, CSS и~Javascript как
повторно используемый компонент.

Наш виджет будет требовать один аргумент: функцию преобразования URL для
подсайта~\lstinline'Chat' в URL основного сайта. Обоснование следующее:
разработчик приложения может поместить подсайт чата в любом месте своей
структуры URL, а виджету требуется генерировать Javascript, который должен
указывать на корректные URL. Итак, приступим:
\begin{code}
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> WidgetT master IO ()
chatWidget toMaster = do
\end{code}

Далее нам нужно сгенерировать несколько идентификаторов, которые будет
использовать наш виджет. Хорошая практика~--- позволить Yesod
сгенерировать уникальные идентификаторы, вместо ручного их создания,
во избежание коллизий с именами.
\begin{code}
    chat <- newIdent   -- обрамляющий div
    output <- newIdent -- элемент, где отображаются сообщения
    input <- newIdent  -- поле пользовательского ввода
\end{code}

И теперь нам нужно проверить, выполнил ли пользователь вход на сайт, используя
функцию~\lstinline'isLoggedIn' из нашего класса типов~\lstinline'YesodChat'.
Так как мы находится в монаде~\lstinline'Widget', а функция живёт в
монаде~\lstinline'Handler', необходимо использовать
функцию~\lstinline'handlerToWidget'.
\begin{code}
    ili <- handlerToWidget isLoggedIn  -- проверить, выполнили ли мы вход
\end{code}

Если пользователь зашёл на сайт, мы хотим отобразить блок чата, добавить ему
стиля, используя CSS, и сделать интерактивным с помощью Javascript.  Это
большей частью клиентский код, завёрнутый в~\lstinline'Widget':
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

И, наконец, если пользователь не выполнил вход, мы просим его сделать это,
чтобы пользоваться чатом.
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
