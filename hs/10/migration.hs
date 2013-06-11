{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkSave "entityDefs"] [persist|
Person
    name String
    age Int
    deriving Show
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration $ migrate entityDefs (undefined :: Person) -- добавлена эта строчка, и только!
    michaelId <- insert $ Person "Michael" 26
    michael <- get michaelId
    liftIO $ print michael
