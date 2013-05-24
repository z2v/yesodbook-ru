{-# LANGUAGE OverloadedStrings #-}
import Text.XML
import Prelude hiding (writeFile)

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" []
        [ NodeElement $ Element "head" []
            [ NodeElement $ Element "title" []
                [ NodeContent "My "
                , NodeElement $ Element "b" []
                    [ NodeContent "Title"
                    ]
                ]
            ]
        , NodeElement $ Element "body" []
            [ NodeElement $ Element "p" []
                [ NodeContent "foo bar baz"
                ]
            ]
        ]
