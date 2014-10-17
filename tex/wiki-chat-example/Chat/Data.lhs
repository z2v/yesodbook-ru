\ignore{
\begin{code}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
\end{code}
}

\subsection{Данные}
Чтобы определить подсайт, нам сначала необходимо создать тип-основание для подсайта,
так же как мы сделали бы для обычного приложения Yesod. В нашем случае, мы хотим
хранить канал для всех событий, которые должны быть отправлены отдельным участникам
чата. Будет это выглядеть как-то так:
\begin{code}
module Chat.Data where

import           Control.Concurrent.Chan (Chan)
import           Network.Wai.EventSource (ServerEvent)
import           Yesod

--- | Тип-основание нашего подсайта. Мы поддерживаем канал событий,
--- который будет совместно использоваться всеми подключениями.
data Chat = Chat (Chan ServerEvent)
\end{code}

Мы должны определить маршруты для нашего подсайта в том же модуле. Нам
потребуются две команды: для отправки нового сообщения всем пользователям и для
получения потока сообщений.

\begin{code}
mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]
\end{code}
