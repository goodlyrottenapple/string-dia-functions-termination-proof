{-# LANGUAGE  ScopedTypeVariables #-}
module Main where

import Protolude
import qualified Prelude as P
import Strings
import Strings.Rules
import Strings.SVG

import Lucid.Svg(Svg)
import Lucid.Base(Html,toHtml,renderToFile)
import Lucid.Html5
import qualified Data.Set as S


printEquivs :: Diagram -> Svg ()
printEquivs dia = do
    p_ "These are the equivalences for:"
    svgDia dia
    br_ []
    forM_ (S.toList $ equivs dia) (\x -> svgDia x >> br_ [])




printApplicableRules :: [Rule] -> Diagram -> Svg ()
printApplicableRules rs dia = do
    p_ "These are the applicable rules for:"
    svgDia dia
    br_ []
    forM_ (S.toList $ collapseEquivs $ applyRulesEquiv rs dia) (\x -> svgDia x >> br_ [])




printTree' :: Tree Diagram -> Svg ()
printTree' (Leaf dia) = li_ $ div_ $ svgDia dia
printTree' (Node dia xs) = 
    li_ $ do
        div_ (svgDia dia)
        ul_ (forM_ xs printTree')

printTree :: (Tree Diagram, Int) -> Svg ()
printTree (x ,id) = div_ [class_ ("section " <> isConfluent), id_ (show id)] (div_ [class_ "tree"] $ do
    a_ [href_ "#home"] $ button_ $ span_ [class_ "icon-up"] $ return ()
    ul_ (printTree' x))
    where
        isConfluent = if (length $ collectFromLeaves x) > 1 then "notconfluent" else "confluent"

criticalPeaksOrig :: [Diagram]
criticalPeaksOrig = map pretty [
        twist_ 0     <> twist_ 0     <> twist_ 0 ,
        cup_   0     <> twist_ 0     <> twist_ 0 ,
        twist_ 0     <> cup_   0     <> twist_ 0 ,
        cup_   0     <> cup_   0     <> twist_ 0 ,
        lolly  0 'b' <> twist_ 0     <> twist_ 0 ,
        lolly  1 'b' <> twist_ 0     <> twist_ 0 ,
        lolly  0 'a' <> lolly  0 'b' <> twist_ 0 ,

        twist_ 0     <> twist_ 0     <> cup_   0 ,
        cup_   0     <> twist_ 0     <> cup_   0 ,
        twist_ 0     <> cup_   0     <> cup_   0 ,
        cup_   0     <> cup_   0     <> cup_   0 ,
        lolly  0 'b' <> twist_ 0     <> cup_   0 ,
        lolly  1 'b' <> twist_ 0     <> cup_   0 ,
        lolly  0 'b' <> lolly  1 'a' <> cup_   0 ,


        lolly  0 'c' <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        lolly  1 'c' <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        lolly  2 'c' <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        lolly  0 'c' <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        lolly  1 'c' <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        lolly  2 'c' <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        lolly  0 'c' <> cup_   0     <> twist_ 0 ,
        lolly  1 'c' <> cup_   0     <> twist_ 0 ,
        lolly  2 'c' <> cup_   0     <> twist_ 0 ,


        lolly  0 'c' <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        lolly  1 'c' <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        lolly  2 'c' <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        lolly  0 'c' <> twist_ 0     <> cup_   1     <> cup_   0 ,
        lolly  1 'c' <> twist_ 0     <> cup_   1     <> cup_   0 ,
        lolly  2 'c' <> twist_ 0     <> cup_   1     <> cup_   0 ,
        lolly  0 'c' <> cup_   0     <> cup_   0 ,
        lolly  1 'c' <> cup_   0     <> cup_   0 ,
        lolly  2 'c' <> cup_   0     <> cup_   0 ,


        twist_ 0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 0 ,
        twist_ 0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 0 ,
        cup_   0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        cup_   0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> cup_   0     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,

        twist_ 0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   0 ,
        twist_ 0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   0 ,
        cup_   0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        cup_   0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> cup_   0     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,

        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> twist_ 0 ,

        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> cup_   0

    ]

criticalPeaks :: [Diagram]
criticalPeaks = S.toList $ S.fromList $ map (P.head . S.toList . equivs) $ map pretty $ concat [
       [twist_ 0, cup_ 0, lolly 0 'c', lolly 1 'c', diamond 0 ' ' 'c', diamond 1 ' ' 'c'] `prod` [twist_ 0, cup_ 0] `prod` [twist_ 0, cup_ 0],
       
       [lolly 0 'c' <> lolly 1 'd', diamond 0 ' ' 'c' <> diamond 1 ' ' 'd'] `prod` [twist_ 0, cup_ 0],

       [lolly 0 'c', lolly 1 'c', lolly 2 'c', diamond 0 ' ' 'd', diamond 1 ' ' 'd', diamond 2 ' ' 'd'] `prod` [(twist_ 0 <> twist_ 1), (twist_ 0 <> cup_ 1), cup_ 0] `prod` [twist_ 0, cup_ 0],

       [diamond 0 'a' 'b'] `prod` (concat [[diamond 0 ' ' 'a'] `prod` [diamond 0 ' ' 'b', diamond 0 ' ' 'd'] , [diamond 0 ' ' 'c'] `prod` [diamond 0 ' ' 'a' , diamond 0 ' ' 'b', diamond 0 ' ' 'd']]),


       [twist_ 0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 0 ,
        twist_ 0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 0 ,
        cup_   0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        cup_   0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> cup_   0     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,

        twist_ 0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   0 ,
        twist_ 0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   0 ,
        cup_   0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        cup_   0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> cup_   0     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,

        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> twist_ 0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> twist_ 0 ,

        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> cup_   0 ,
        twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> cup_   0]

    ]
    where
        prod :: Semigroup a => [a] -> [a] -> [a]
        prod _ [] = []
        prod as (b:bs) = map (<> b) as ++ prod as bs 


-- criticalPeaks = S.toList $ S.fromList $ map (P.head . S.toList . equivs) $ map pretty $ concat [
--        [twist_ 0, cup_ 0, lolly 0 'c', lolly 1 'c'] `prod` [twist_ 0, cup_ 0] `prod` [twist_ 0, cup_ 0],
       
--        [lolly 0 'c' <> lolly 1 'd'] `prod` [twist_ 0, cup_ 0],

--        [lolly 0 'c', lolly 1 'c', lolly 2 'c'] `prod` [(twist_ 0 <> twist_ 1), (twist_ 0 <> cup_ 1), cup_ 0] `prod` [twist_ 0, cup_ 0],

--        -- [twist_ 0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 0 ,
--        --  cup_   0     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
--        --  cup_   0     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
--        --  twist_ 0     <> cup_   1     <> cup_   0     <> twist_ 0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,

--        --  twist_ 0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   0 ,
--        --  cup_   0     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
--        --  cup_   0     <> twist_ 0     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> cup_   1     <> cup_   0     <> cup_   0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> cup_   1     <> twist_ 0     <> cup_   1     <> cup_   0 ,

--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> twist_ 0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> twist_ 0 ,

--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> twist_ 1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> twist_ 1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> twist_ 2     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> twist_ 0     <> cup_   2     <> cup_   1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> twist_ 1     <> cup_   0 ,
--        --  twist_ 0     <> twist_ 1     <> cup_   0     <> cup_   1     <> cup_   0]
--     ]

printCriticalPeaks :: [Diagram] -> Html ()
printCriticalPeaks ps = do
    div_ [id_ "home"] $ do
        h1_ "Confluence proofs"
        p_ $ toHtml $ "There are " ++ (show $ length ps) ++ 
            " critical peaks. To see the confluence proof, click on corresponding diagram below:"
        p_ $ i_ "Note: Some diagrams appear to only have one branch. This is because the diagram reduces to the same diagram via multiple different rules in one step."
        -- "Note: The proofs are " <> mark_ [class_ "green"] "green" 
            -- <> " if confluent, and " <> mark_ [class_ "red"] "red" <> " otherwise."
    div_ $ do
        forM_ (zip ps [0..]) (\(dia,(i :: Int)) -> a_ [href_ ("#" <> show i)] $ button_ [class_ "dia-button"] $ svgDia dia)

    
    div_ [class_ "card"] $ do
        forM_ (zip (map generateAllReductionPaths ps) [0..]) printTree
        

page :: Html () -> Html ()
page content = html_ $ do
    head_ $ do
      title_ "Confluence checking - output"
      link_ [rel_ "stylesheet",type_ "text/css",href_ "style.css"]
      link_ [rel_ "stylesheet",type_ "text/css",href_ "mini-default.min.css"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    body_ $ do

        
        content

        script_ (
            "var svgs = document.getElementsByTagName(\"svg\");" <>
            "for (var i = svgs.length - 1; i >= 0; i--) {"<>
            "var svg = svgs[i];" <>
            "var bbox = svg.getBBox();" <>
            "svg.setAttribute(\"viewBox\", (bbox.x-10)+\" \"+(bbox.y-10)+\" \"+(bbox.width+20)+\" \"+(bbox.height+20));" <>
            "svg.setAttribute(\"width\", (bbox.width+20)  + \"px\");" <>
            "svg.setAttribute(\"height\",(bbox.height+20) + \"px\");" <>
            "}")

main :: IO ()
main = renderToFile "../docs/confluence.html" $ page $ printCriticalPeaks criticalPeaks
