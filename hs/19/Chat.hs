 -- @Chat.hs
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts
  #-}
-- | Этот модуль определяет подсайт, который позволяет вам вставлять
-- элемент управления с чатом в любую страницу вашего сайта. Он использует
-- eventsource для отправки сообщений браузеру с сервера.
module Chat where

import Yesod
import Control.Concurrent.Chan (Chan, dupChan, writeChan)
import Data.Text (Text)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Language.Haskell.TH.Syntax (Type (VarT), Pred (ClassP), mkName)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Monoid (mappend)
import Data.Aeson (toJSON)

-- | Тип-основание нашего подсайта. Мы поддерживаем канал событий,
-- который будет совместно использоваться всеми подключениями.
data Chat = Chat (Chan ServerEvent)

-- | Нам нужно иметь возможность проверять, что пользователь выполнил вход,
-- и получать его имя (для вывода сообщений).
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: GHandler sub master Text
    isLoggedIn :: GHandler sub master Bool

-- Теперь создадим наш подсайт. Первый аргумент - это подсайт, что
-- очень напоминает то, как мы раньше использовали mkYesod. Второй аргумент
-- характерен для создания подсайта. В данном случае он означает "основной сайт
-- должен быть экземпляром YesodChat".
--
-- Мы определяем два маршрута: маршрут отправки сообщений от клиента серверу
-- и маршрут открытия потока событий для приёма сообщений от сервера.
mkYesodSub "Chat"
    [ ClassP ''YesodChat [VarT $ mkName "master"]
    ] [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

-- | Получить сообщение от пользователя и отправить его всем слушателям.
postSendR :: YesodChat master => GHandler Chat master ()
postSendR = do
    from <- getUserName

    -- Обратите внимание, что мы используем параметры GET для упрощения
    -- кода Ajax. Можно легко переключиться на POST. Тем не менее, общий
    -- наш подход всё же является RESTful, так как доступ к этому маршруту
    -- можно получить только через POST-запрос.
    body <- runInputGet $ ireq textField "message"

    -- Получить канал
    Chat chan <- getYesodSub

    -- Отправить событие с именем пользователя и сообщением всем слушателям.
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from `mappend` fromText ": " `mappend` fromText body

-- | Отправить eventstream-ответ, в который включены все сообщения.
getReceiveR :: GHandler Chat master ()
getReceiveR = do
    -- Сначала получаем главный канал
    Chat chan0 <- getYesodSub

    -- Мы сделали копию канала, что позволяет нам создавать широковещательные каналы.
    chan <- liftIO $ dupChan chan0

    -- Теперь воспользуемся API источника событий. eventSourceAppChan
    -- принимает два параметра: канал событий, из которого выполнять чтение,
    -- и запрос WAI. Он возвращает отклик WAI, который мы можем возвратить
    -- с помощью sendWaiResponse.
    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req
    sendWaiResponse res

-- | Предоставим виджет, который основной сайт сможет внедрить в любую страницу.
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> GWidget sub master ()
-- Аргумент toMaster говорит нам как преобразовывать Route Chat в маршрут
-- основного сайта. Вы можете счесть эту информация избыточной, но использование
-- такого подхода даёт нам возможность иметь несколько подсайтов с чатом в пределах
-- одного сайта.
chatWidget toMaster = do
    -- Получим некоторые уникальные идентификаторы, помогающие в создании
    -- наших HTML/CSS. Вспомним, что у нас нет представления о том, как
    -- выглядит HTML основного сайта. Поэтому мы не можем предполагать, что способны
    -- придумать идентификаторы, которые не будут использоваться повторно.
    -- Кроме того, несколько экземпляров chatWidget могут быть внедрены в одну и ту же страницу.
    chat <- lift newIdent   -- обрамляющий div
    output <- lift newIdent -- элемент, где отображаются сообщения
    input <- lift newIdent  -- поле пользовательского ввода

    ili <- lift isLoggedIn  -- проверить, выполнили ли мы вход
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
            -- И теперь вот такой JavaScript
            toWidgetBody [julius|
// Настроим принимающую сторону
var output = document.getElementById(#{toJSON output});
var src = new EventSource("@{toMaster ReceiveR}");
src.onmessage = function(msg) {
    // Эта функция будет вызвана для каждого нового сообщения
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
        else do
            -- Пользователь не выполнил вход, выдать об этом сообщение.
            master <- lift getYesod
            [whamlet|
<p>
    Вы должны #
    $maybe ar <- authRoute master
        <a href=@{ar}>выполнить вход
    $nothing
        выполнить вход
    \ чтобы воспользоваться чатом.
|]
