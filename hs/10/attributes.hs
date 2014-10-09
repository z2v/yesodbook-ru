{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    time <- liftIO getCurrentTime
    runMigration migrateAll
    insert $ Person "Michael" (Just 26) time
    insert $ Person "Greg" Nothing time
    return ()
