{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, FlexibleContexts #-}

import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkPersist, persistUpperCase, share, mkMigrate, sqlSettings)
import Database.Persist.GenericSql (runSqlConn, runMigration, SqlPersist)
import Database.Persist.GenericSql.Raw (withStmt)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Store (PersistValue)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
    name Text
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    insert $ Person "Michael Snoyman"
    insert $ Person "Miriam Snoyman"
    insert $ Person "Eliezer Snoyman"
    insert $ Person "Gavriella Snoyman"
    insert $ Person "Greg Weber"
    insert $ Person "Rick Richardson"

    -- Persistent не предоставляет ключевого слова LIKE, но нам хотелось бы
    -- получить всю семью Snoyman'ов...
    let sql = "SELECT name FROM Person WHERE name LIKE '%Snoyman'"
    C.runResourceT $ withStmt sql []
                C.$$ CL.mapM_ $ liftIO . print
