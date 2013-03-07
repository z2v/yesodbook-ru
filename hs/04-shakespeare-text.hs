{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text (Text)
import Control.Monad (forM_)

data Item = Item
    { itemName :: Text
    , itemQty :: Int
    }

items :: [Item]
items =
    [ Item "яблоки" 5
    , Item "бананы" 10
    ]

main :: IO ()
main = forM_ items $ \item -> TLIO.putStrLn
    [lt|У вас есть #{show $ itemQty item} #{itemName item}.|]
