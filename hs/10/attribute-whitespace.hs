{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    language String default='Haskell'
    country String "default='Российская Федерация'"
    deriving Show
|]

main = runSqlite ":memory:" $ do
    runMigration migrateAll
