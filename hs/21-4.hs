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
