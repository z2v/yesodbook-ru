{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Exception (IOException, try)
import           Control.Monad     (when)
import           Yesod

data App = App
instance Yesod App where
    -- Эта функция контролирует, какие сообщения протоколировать
    shouldLog App src level =
        True -- подходит для разработки
        -- level == LevelWarn || level == LevelError -- подходит для продуктива

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
    $logDebug "Пытаемся прочитать файл с данными"
    edata <- liftIO $ try $ readFile "datafile.txt"
    case edata :: Either IOException String of
        Left e -> do
            $logError $ "Не удалось прочитать файл"
            defaultLayout [whamlet|Возникла ошибка|]
        Right str -> do
            $logInfo "Файл успешно прочитан"
            let ls = lines str
            when (length ls < 5) $ $logWarn "Меньше 5-ти строк данных"
            defaultLayout
                [whamlet|
                    <ol>
                        $forall l <- ls
                            <li>#{l}
                |]

main :: IO ()
main = warp 3000 App
