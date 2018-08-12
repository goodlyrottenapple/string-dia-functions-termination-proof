{-# LANGUAGE RecordWildCards #-}

module Strings.SVG(svgDia) where

import Protolude
import Prelude((!!))
import Strings

import Lucid.Svg
import Lucid.Base(toHtml)

-- quarter of a width of a diagram
qWidth :: Int
qWidth = 8



line :: Monad m => Int -> Int -> Int -> Int -> SvgT m ()
line x1 y1 x2 y2 = line_ [x1_ (show x1), y1_ (show y1), x2_ (show x2), y2_ (show y2), stroke_ "black", stroke_width_ "2", stroke_linecap_ "round",
  stroke_linejoin_ "round"]

lline :: Monad m => Bool-> Char -> Int -> Int -> Int -> Int -> SvgT m ()
lline isStart l x1 y1 x2 y2 = do
    if isStart then label x1 (y1-5) l else return ()
    line x1 y1 x2 y2

label :: Monad m => Int -> Int -> Char -> SvgT m ()
label x y c = text_ [
        x_ (show x), 
        y_ (show y), 
        font_size_ "20", 
        fill_ "black"
        --, transform_ "translate(0,-500)"
        -- transform_ ("rotate(180, " <> (show x) <> ", " <> (show y) <> ")")
    ] (toHtml [c])


drawTwist :: Int -> Int -> Int -> Svg ()
drawTwist height posX posY= do
    line posX            posY          (posX+qWidth)   posY
    -- if isStart then label posX (posY+15) a else return ()

    line posX            (posY+height) (posX+qWidth)   (posY+height)
    -- if isStart then label posX (posY+height-5) b else return ()

    line (posX+qWidth)   (posY)        (posX+3*qWidth) (posY+height)
    line (posX+qWidth)   (posY+height) (posX+3*qWidth) (posY)

    line (posX+3*qWidth) posY          (posX+4*qWidth)   posY
    line (posX+3*qWidth) (posY+height) (posX+4*qWidth)   (posY+height)


drawCup :: Char -> Int -> Int -> Int -> Svg ()
drawCup b height posX posY= do
    line posX            posY                (posX+qWidth)   posY
    -- if isStart then label posX (posY+15) a else return ()
    line posX            (posY+height)       (posX+qWidth)   (posY+height)
    -- if isStart then label posX (posY+height-5) b else return ()

    line (posX+qWidth)   (posY)              (posX+2*qWidth) (posY+height`div`2)
    line (posX+qWidth)   (posY+height)       (posX+2*qWidth) (posY+height`div`2)

    line (posX+2*qWidth) (posY+height`div`2) (posX+4*qWidth) (posY+height`div`2)
    label (posX+2*qWidth+5) (posY+height`div`2-5) b


drawDiamond :: Char -> Int -> Int -> Svg ()
drawDiamond b posX posY= do
    line posX            posY                (posX+4*qWidth)   posY
    -- if isStart then label posX (posY+15) a else return ()
    rect_ [
        x_ (show sqX), 
        y_ (show sqY), 
        width_ (show qWidth), 
        height_ (show qWidth),
        stroke_ "black",
        fill_ "white",
        transform_ ("rotate(45, " <> (show $ sqX+qWidth`div`2) <> ", " <> (show $ sqY+qWidth`div`2) <> ")"),
        stroke_width_ "2"]
    label (posX+4*qWidth-10) (posY+15) b
    where
        sqX = posX+2*qWidth-qWidth`div`2
        sqY = posY-qWidth`div`2

drawLolly :: Char -> Int -> Int -> Svg ()
drawLolly a posX posY= do
    line sqX            posY                (posX+4*qWidth)   posY
    -- label posX (posY+15) a
    circle_ [
        cx_ (show sqX), 
        cy_ (show sqY), 
        r_ (show $ qWidth`div`2), 
        stroke_ "black",
        -- fill_ "white",
        -- transform_ ("rotate(45, " <> (show $ sqX+qWidth`div`2) <> ", " <> (show $ sqY+qWidth`div`2) <> ")"),
        stroke_width_ "2"]
    label (posX+4*qWidth-10) (posY+15) a
    where
        sqX = posX+2*qWidth
        sqY = posY+qWidth`div`8


updatePorts :: Int -> Generator -> [Int] -> [Int]
updatePorts _ Twist ps = ps
updatePorts _ (Diamond _) ps = ps
updatePorts n Cup ps =  take n ps ++ [ps!!n + (ps!!(n+1) - ps!!n)`div`2] ++ drop (n+2) ps
updatePorts n (Lolly _) ps = take n ps ++ [posY] ++ drop n ps
    where
        posY = ps!!n + (if n > 0 then (ps!!(n-1) - ps!!n)`div`2 else -2*qWidth)


dia2Svg' :: [Char] -> Int -> [Int] -> Bool -> [(Int,Generator)] -> Svg ()
dia2Svg' _ _ _ False [] = return ()
dia2Svg' portLabels posX ports True xs = do
    forM_ (zip (take (length portLabels) ports) (reverse portLabels)) (\(y,a) -> lline True a posX y (posX+4*qWidth) y)
    dia2Svg' portLabels (posX+4*qWidth) ports False xs

dia2Svg' portLabels posX ports _ ((n',Twist):xs) = do
    forM_ (take n ports) (\y -> line posX y (posX+4*qWidth) y)
    drawTwist (ports!!(n+1) - ports!!n) posX (ports!!n)
    forM_ (drop (n+2) $ take maxH ports) (\y -> line posX y (posX+4*qWidth) y)
    dia2Svg' portLabels' (posX+4*qWidth) ports False xs

    where
        n = maxH -n'-2
        maxH = length portLabels
        portLabels' = updateInPorts n' Twist portLabels


dia2Svg' portLabels posX ports _ ((n',Diamond b):xs) = do
    forM_ (take n ports) (\y -> line posX y (posX+4*qWidth) y)
    drawDiamond b posX (ports!!n)
    forM_ (drop (n+1) $ take maxH ports) (\y -> line posX y (posX+4*qWidth) y)
    dia2Svg' portLabels' (posX+4*qWidth) ports False xs

    where
        n = maxH -n'-1
        maxH = length portLabels
        portLabels' = updateInPorts n' (Diamond b) portLabels 


dia2Svg' portLabels posX ports _ ((n',Cup):xs) = do
    forM_ (take n ports) (\y -> line posX y (posX+4*qWidth) y)
    drawCup b (ports!!(n+1) - ports!!n) posX (ports!!n)
    forM_ (drop (n+2) $ take maxH ports) (\y -> line posX y (posX+4*qWidth) y)
    dia2Svg' portLabels' (posX+4*qWidth) ports' False xs

    where
        ports' = updatePorts n Cup ports -- take n ports ++ [ports!!n + (ports!!(n+1) - ports!!n)`div`2] ++ drop (n+2) ports
        portLabels' = updateInPorts n' Cup portLabels
        n = maxH -n'-2
        maxH = length portLabels
        -- a = portLabels !! n
        b = portLabels' !! n'

dia2Svg' portLabels posX ports _ ((n',Lolly a):xs) = do
    forM_ (take n ports) (\y -> line posX y (posX+4*qWidth) y)
    -- forM_ (zip (take n ports) (take n portLabels)) (\(y,a) -> lline isStart a posX y (posX+4*qWidth) y)
    drawLolly a posX posY
    -- forM_ (zip (drop n $ take maxH ports) (drop n $ take maxH portLabels)) (\(y,a) -> lline isStart a posX y (posX+4*qWidth) y)

    forM_ (drop n $ take maxH ports) (\y -> line posX y (posX+4*qWidth) y)
    dia2Svg' portLabels' (posX+4*qWidth) ports' False xs

    where
        n = maxH-n'
        maxH = length portLabels
        ports' = updatePorts n (Lolly a) ports-- take n ports ++ [posY] ++ drop n ports
        portLabels' = updateInPorts n' (Lolly a) portLabels
        posY = ports!!n + (if n > 0 then (ports!!(n-1) - ports!!n)`div`2 else -2*qWidth)

noOfLollies :: Diagram -> Int
noOfLollies Diagram{..} = noOfLollies' d 
    where
        noOfLollies' [] = 0
        noOfLollies' ((_,Lolly _):xs) = 1 + noOfLollies' xs
        noOfLollies' (_:xs) = noOfLollies' xs


mkPorts :: Int -> [Int]
mkPorts m = ports'
    where 
        ports' = 0 : map (+ (4*m*qWidth)) ports'


dia2Svg :: Diagram -> Svg ()
dia2Svg dia@Diagram{..} = dia2Svg' inPorts 0 (mkPorts $ noOfLollies dia+1) True d


diaMaxHeight :: Diagram -> Int
diaMaxHeight dia@(Diagram inPs d) = ps!! (length inPs')
    where
        inPs' = foldr (\(n,g) ps' -> updateInPorts n g ps') inPs d
        ps = foldr (\(n,g) ps' -> updatePorts n g ps') (mkPorts $ noOfLollies dia+1) d



svgDia :: Diagram -> Svg ()
svgDia dia =
  with (svg11_ $ g_ [
        transform_ ("translate(0 " <> (show $ 4*qWidth+5) <> ")")
    ] (dia2Svg dia)) [width_ (show $ 4*qWidth*(length (d dia)+1)) , height_ (show $ (diaMaxHeight dia)+20)]
  -- height_ (show $ (4*(noOfLollies dia+1)*qWidth)*(diaMaxHeight dia)+20)



