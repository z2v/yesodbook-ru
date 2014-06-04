\subsection{Данные}

Now we can proceed with writing our main application. This application will
include the chat subsite and a wiki. The first thing we need to consider is how
to store the wiki contents. Normally, we'd want to put this in some kind of a
Persistent database. For simplicity, we'll just use an in-memory
representation. Each Wiki page is indicated by a list of names, and the contents of each page is going to be a piece of +Text+. So our full foundation datatype is:

\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Chat
import           Control.Concurrent.Chan (newChan)
import qualified Data.IORef              as I
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as TL
import           Text.Markdown           (def, markdown)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy        (authDummy)
\end{code}
}

\begin{code}
data App = App
    { getChat     :: Chat
    , wikiContent :: I.IORef (Map.Map [Text] Text)
    }
\end{code}

Next we want to set up our routes:

\begin{code}
mkYesod "App" [parseRoutes|
/            HomeR GET      -- the homepage
/wiki/*Texts WikiR GET POST -- note the multipiece for the wiki hierarchy

/chat        ChatR Chat getChat    -- the chat subsite
/auth        AuthR Auth getAuth    -- the auth subsite
|]
\end{code}

\subsection{Экземляры классов типов}

We need to make two modifications to the default +Yesod+ instance. Firstly, we
want to provide an implementation of +authRoute+, so that our chat subsite
widget can provide a proper link to a login page. Secondly, we'll provide a
override to the +defaultLayout+. Besides providing login/logout links, this
function will add in the chat widget on every page.

\begin{code}
instance Yesod App where
    authRoute _ = Just $ AuthR LoginR -- get a working login link

    -- Our custom defaultLayout will add the chat widget to every page.
    -- We'll also add login and logout links to the top.
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
            chatWidget ChatR
        mmsg <- getMessage
        giveUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <div .message>#{msg}
                        <nav>
                            <a href=@{AuthR LoginR}>Login
                            \ | #
                            <a href=@{AuthR LogoutR}>Logout
                        ^{pageBody pc}
            |]
\end{code}

Since we're using the chat subsite, we have to provide an instance of
+YesodChat+.

\begin{code}
instance YesodChat App where
    getUserName = do
        muid <- maybeAuthId
        case muid of
            Nothing -> do
                setMessage "Not logged in"
                redirect $ AuthR LoginR
            Just uid -> return uid
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma
\end{code}

Our +YesodAuth+ and +RenderMessage+ instances, as well as the homepage handler,
are rather bland:

\begin{code}
-- Fairly standard YesodAuth instance. We'll use the dummy plugin so that you
-- can create any name you want, and store the login name as the AuthId.
instance YesodAuth App where
    type AuthId App = Text
    authPlugins _ = [authDummy]
    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authHttpManager = error "authHttpManager" -- not used by authDummy
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Nothing special here, just giving a link to the root of the wiki.
getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Welcome to the Wiki!
        <p>
            <a href=@{wikiRoot}>Wiki root
    |]
  where
    wikiRoot = WikiR []
\end{code}

\subsection{Обработчики вики}

Now it's time to write our wiki handlers: a GET for displaying a page, and a
POST for updating a page. We'll also define a +wikiForm+ function to be used on
both handlers:

\begin{code}
-- A form for getting wiki content
wikiForm :: Maybe Textarea -> Html -> MForm Handler (FormResult Textarea, Widget)
wikiForm mtext = renderDivs $ areq textareaField "Page body" mtext

-- Show a wiki page and an edit form
getWikiR :: [Text] -> Handler Html
getWikiR page = do
    -- Get the reference to the contents map
    icontent <- fmap wikiContent getYesod

    -- And read the map from inside the reference
    content <- liftIO $ I.readIORef icontent

    -- Lookup the contents of the current page, if available
    let mtext = Map.lookup page content

    -- Generate a form with the current contents as the default value.
    -- Note that we use the Textarea wrapper to get a <textarea>.
    (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- We're treating the input as markdown. The markdown package
            -- automatically handles XSS protection for us.
            Just text -> toWidget $ markdown def $ TL.fromStrict text
            Nothing -> [whamlet|<p>Page does not yet exist|]
        [whamlet|
            <h2>Edit page
            <form method=post>
                ^{form}
                <div>
                    <input type=submit>
        |]

-- Get a submitted wiki page and updated the contents.
postWikiR :: [Text] -> Handler Html
postWikiR page = do
    icontent <- fmap wikiContent getYesod
    content <- liftIO $ I.readIORef icontent
    let mtext = Map.lookup page content
    ((res, form), _) <- runFormPost $ wikiForm $ fmap Textarea mtext
    case res of
        FormSuccess (Textarea t) -> do
            liftIO $ I.atomicModifyIORef icontent $
                \m -> (Map.insert page t m, ())
            setMessage "Page updated"
            redirect $ WikiR page
        _ -> defaultLayout
                [whamlet|
                    <form method=post>
                        ^{form}
                        <div>
                            <input type=submit>
                |]
\end{code}

\subsection{Запуск}

Finally, we're ready to run our application. Unlike many of our previous
examples in this book, we need to perform some real initialization in the
+main+ function. The +Chat+ subsite requires an empty +Chan+ to be created, and
we need to create a mutable variable to hold the wiki contents. Once we have
those values, we can create an +App+ value and pass it to the +warp+ function.

\begin{code}
main :: IO ()
main = do
    -- Create our server event channel
    chan <- newChan

    -- Initially have a blank database of wiki pages
    icontent <- I.newIORef Map.empty

    -- Run our app
    warpEnv App
        { getChat = Chat chan
        , wikiContent = icontent
        }
\end{code}
