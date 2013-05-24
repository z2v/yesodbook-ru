{-# LANGUAGE OverloadedStrings #-}
import Text.XML
import Prelude hiding (writeFile)
import Data.Map (empty)

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty
        [ NodeElement $ Element "head" empty
            [ NodeElement $ Element "title" empty
                [ NodeContent "My "
                , NodeElement $ Element "b" empty
                    [ NodeContent "Title"
                    ]
                ]
            ]
        , NodeElement $ Element "body" empty
            [ NodeElement $ Element "p" empty
                [ NodeContent "foo bar baz"
                ]
            ]
        ]
