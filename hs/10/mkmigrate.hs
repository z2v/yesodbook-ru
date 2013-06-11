{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name String
    age Int
    deriving Show
Car
    color String
    make String
    model String
    deriving Show
|]

main = runSqlite ":memory:" $ do
       runMigration migrateAll
