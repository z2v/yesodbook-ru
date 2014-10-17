{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Определяем наши сущности, как обычно
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- Мы храним пул соединений в основном типе. Когда программа
-- инициализируется, мы создаем начальный пул, и каждый раз, когда нам
-- нужно произвести действие, мы выделяем соединение из пула
data PersistTest = PersistTest ConnectionPool

-- Мы создаем один-единственный маршрут для доступа к человеку. Это довольно распространенная
-- практика, когда в маршрутах используется Id.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
|]

-- Тут ничего особенного
instance Yesod PersistTest

-- Теперь нам нужно определить экземпляр класса YesodPersist, который будет
-- следить за тем, какой бэкенд мы используем и как следует выполнять
-- действия
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlPersistT

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- Выводим список всех людей в базе
getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]

-- Мы просто возвращаем строковое представление челавека
-- или ошибку 404, если такой Person не существует
getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool
