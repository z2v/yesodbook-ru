{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Привет, меня зовут #{name} и мне #{age} лет.
        |]
    provideJson person
  where
    person@Person {..} = Person "Michael" 28

main :: IO ()
main = warp 3000 App
