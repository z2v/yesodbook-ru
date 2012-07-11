getRootR :: Handler RepHtml
getRootR = do
    docCount <- runDB $ count ([] :: [Filter Doc])
    ((_, docWidget), _) <- runFormPost addDocForm
    ((_, searchWidget), _) <- runFormGet searchForm
    let docs = if docCount == 1
                then "There is currently 1 document."
                else "There are currently " ++ show docCount ++ " documents."
    defaultLayout [whamlet|
<p>Welcome to the search application. #{docs}
<form method=post action=@{AddDocR}>
    <table>
        ^{docWidget}
        <tr>
            <td colspan=3>
                <input type=submit value="Add document">
<form method=get action=@{SearchR}>
    ^{searchWidget}
    <input type=submit value=Search>
|]

postAddDocR :: Handler RepHtml
postAddDocR = do
    ((res, docWidget), _) <- runFormPost addDocForm
    case res of
        FormSuccess doc -> do
            docid <- runDB $ insert doc
            setMessage "Document added"
            redirect $ DocR docid
        _ -> defaultLayout [whamlet|
<form method=post action=@{AddDocR}>
    <table>
        ^{docWidget}
        <tr>
            <td colspan=3>
                <input type=submit value="Add document">
|]

getDocR :: DocId -> Handler RepHtml
getDocR docid = do
    doc <- runDB $ get404 docid
    defaultLayout $
        [whamlet|
<h1>#{docTitle doc}
<div .content>#{docContent doc}
|]
data Result = Result
    { resultId :: DocId
    , resultTitle :: Text
    , resultExcerpt :: Html
    }

getResult :: DocId -> Doc -> Text -> IO Result
getResult docid doc qstring = do
    excerpt' <- S.buildExcerpts
        excerptConfig
        [T.unpack $ escape $ docContent doc]
        "searcher"
        (unpack qstring)
    let excerpt =
            case excerpt' of
                ST.Ok bss -> preEscapedToMarkup $ decodeUtf8With ignore $ L.concat bss
                _ -> return ()
    return Result
        { resultId = docid
        , resultTitle = docTitle doc
        , resultExcerpt = excerpt
        }
  where
    excerptConfig = E.altConfig { E.port = 9312 }

escape :: Textarea -> Text
escape =
    T.concatMap escapeChar . unTextarea
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = T.singleton c

getResults :: Text -> Handler [Result]
getResults qstring = do
    sphinxRes' <- liftIO $ S.query config "searcher" (unpack qstring)
    case sphinxRes' of
        ST.Ok sphinxRes -> do
            let docids = map (Key . PersistInt64 . ST.documentId) $ ST.matches sphinxRes
            fmap catMaybes $ runDB $ forM docids $ \docid -> do
                mdoc <- get docid
                case mdoc of
                    Nothing -> return Nothing
                    Just doc -> liftIO $ Just <$> getResult docid doc qstring
        _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
        { S.port = 9312
        , S.mode = ST.Any
        }

getSearchR :: Handler RepHtml
getSearchR = do
    ((formRes, searchWidget), _) <- runFormGet searchForm
    searchResults <-
        case formRes of
            FormSuccess qstring -> getResults qstring
            _ -> return []
    defaultLayout $ do
        toWidget [lucius|
.excerpt {
    color: green; font-style: italic
}
.match {
    background-color: yellow;
}
|]
        [whamlet|
<form method=get action=@{SearchR}>
    ^{searchWidget}
    <input type=submit value=Search>
$if not $ null searchResults
    <h1>Results
    $forall result <- searchResults
        <div .result>
            <a href=@{DocR $ resultId result}>#{resultTitle result}
            <div .excerpt>#{resultExcerpt result}
|]

getXmlpipeR :: Handler RepXml
getXmlpipeR = do
    Searcher pool <- getYesod
    let headers = [("Content-Type", "text/xml")]
    managedConn <- lift $ takeResource pool
    let conn = mrValue managedConn
    lift $ mrReuse managedConn True
    let source = fullDocSource conn C.$= renderBuilder def
        flushSource = C.mapOutput C.Chunk source
    sendWaiResponse $ ResponseSource status200 headers flushSource

entityToEvents :: (Entity Doc) -> [X.Event]
entityToEvents (Entity docid doc) =
    [ X.EventBeginElement document [("id", [X.ContentText $ toPathPiece docid])]
    , X.EventBeginElement content []
    , X.EventContent $ X.ContentText $ unTextarea $ docContent doc
    , X.EventEndElement content
    , X.EventEndElement document
    ]

fullDocSource :: Connection -> C.Source (C.ResourceT IO) X.Event
fullDocSource conn = mconcat
    [ CL.sourceList startEvents
    , docSource conn
    , CL.sourceList endEvents
    ]

docSource :: Connection -> C.Source (C.ResourceT IO) X.Event
docSource conn = selectSourceConn conn [] [] C.$= CL.concatMap entityToEvents

toName :: Text -> X.Name
toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset, schema, field, document, content :: X.Name
docset = toName "docset"
schema = toName "schema"
field = toName "field"
document = toName "document"
content = "content" -- no prefix

startEvents, endEvents :: [X.Event]
startEvents =
    [ X.EventBeginDocument
    , X.EventBeginElement docset []
    , X.EventBeginElement schema []
    , X.EventBeginElement field [("name", [X.ContentText "content"])]
    , X.EventEndElement field
    , X.EventEndElement schema
    ]

endEvents =
    [ X.EventEndElement docset
    ]

main :: IO ()
main = withSqlitePool "searcher.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ Searcher pool
