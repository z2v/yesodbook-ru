{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
    firstName String
    lastName String
    age Int
    UniqueName firstName lastName
    deriving Show
|]

main = runSqlite ":memory:" $ do
    runMigration migrateAll
    insert $ Person "Michael" "Snoyman" 26
    michael <- getBy $ UniqueName "Michael" "Snoyman"
    liftIO $ print michael
