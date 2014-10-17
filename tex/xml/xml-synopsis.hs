{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main = do
    -- readFile выбросит исключение времени исполнения для любой ошибки разбора
    -- def использует настройки по умолчанию
    Document prologue root epilogue <- readFile def "input.xml"

    -- root - это корневой элемент документа, давайте модифицируем его
    let root' = transform root

    -- А теперь запишем в файл. Давайте выведем результат с отступами
    writeFile def
        { rsPretty = True
        } "output.html" $ Document prologue root' epilogue

-- Мы преобразуем <document> в документ XHTML
transform :: Element -> Element
transform (Element _name attrs children) = Element "html" M.empty
    [xml|
        <head>
            <title>
                $maybe title <- M.lookup "title" attrs
                    \#{title}
                $nothing
                    Untitled Document
        <body>
            $forall child <- children
                ^{goNode child}
    |]

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- скроем комментарии
goNode (NodeInstruction _) = [] -- и инструкции обработки тоже

-- преобразуем каждый исходный элемент в эквивалент на XHTML
goElem :: Element -> Element
goElem (Element "para" attrs children) =
    Element "p" attrs $ concatMap goNode children
goElem (Element "em" attrs children) =
    Element "i" attrs $ concatMap goNode children
goElem (Element "strong" attrs children) =
    Element "b" attrs $ concatMap goNode children
goElem (Element "image" attrs _children) =
    Element "img" (fixAttr attrs) [] -- у тэга img не может быть дочерних элементов
  where
    fixAttr mattrs
        | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
        | otherwise                 = mattrs
goElem (Element name attrs children) =
    -- не знаем что делать, поэтому просто передаём как есть...
    Element name attrs $ concatMap goNode children
