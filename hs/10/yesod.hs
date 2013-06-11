{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}

import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)

-- Определяем наши сущности, как обычно
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- Мы являемся держателями пула соединений. Когда программа инициализируется, мы
-- создаем начальный пул, и каждый раз, когда нам нужно произвести действие,
-- мы выделяем соединение из пула
data PersistTest = PersistTest ConnectionPool

-- Мы создаем один-единственный маршрут для доступа к человеку. Это довольно распространенная
-- практика, когда в маршрутах используется Id.
mkYesod "PersistTest" [parseRoutes|
/person/#PersonId PersonR GET
|]

-- Тут ничего особенного
instance Yesod PersistTest

-- Теперь нам нужно определить экземпляр класса Yesod Persist, который будет следить
-- за тем, какой бэкенд мы используем и как следует выполнять действия
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlPersist

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- Мы просто возвращаем строковое представление челавека
-- или ошибку 404, если такой Person не существует
getPersonR :: PersonId -> Handler RepPlain
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ RepPlain $ toContent $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runNoLoggingT $ runResourceT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool (insert $ Person "Michael" "Snoyman" 26) pool
    liftIO $ warpDebug 3000 $ PersistTest pool
