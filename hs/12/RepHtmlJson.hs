{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
data R = R
mkYesod "R" [parseRoutes|
/ RootR GET
/#String NameR GET
|]
instance Yesod R

getRootR = defaultLayout $ do
    setTitle "Homepage"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    toWidget [julius|
$(function(){
    $("#ajax a").click(function(){
        jQuery.getJSON($(this).attr("href"), function(o){
            $("div").text(o.name);
        });
        return false;
    });
});
|]
    let names = words "Larry Moe Curly"
    [whamlet|
<h2>AJAX Version
<div #results>
    AJAX results will be placed here when you click #
    the names below.
<ul #ajax>
    $forall name <- names
        <li>
            <a href=@{NameR name}>#{name}

<h2>HTML Version
<p>
    Clicking the names below will redirect the page #
    to an HTML version.
<ul #html>
    $forall name <- names
        <li>
            <a href=@{NameR name}>#{name}

|]

getNameR name = do
    let widget = do
            setTitle $ toHtml name
            [whamlet|Looks like you have Javascript off. Name: #{name}|]
    let json = object ["name" .= name]
    defaultLayoutJson widget json

main = warpDebug 4000 R
