
{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE RecordWildCards #-}

module Strings where

import Protolude
import Prelude((!!))

data Generator = Twist | Diamond Char | Cup | Lolly Char deriving (Show, Eq, Ord)

data Diagram = Diagram {
    inPorts :: [Char]
  , d :: [(Int,Generator)]
} deriving (Show, Eq, Ord)


instance Semigroup Diagram where
    (Diagram is as) <> (Diagram is' as') = Diagram (mergePorts is (shiftPorts as is')) (as ++ as')
        where
            mergePorts [] ys = ys
            mergePorts xs [] = xs
            mergePorts (x:xs)  (_:ys) = x : mergePorts xs ys

            shiftPorts [] ys = ys
            shiftPorts ((_,Cup):xs) ys = shiftPorts xs (' ': ys)
            shiftPorts ((_,Lolly _):xs) ys = shiftPorts xs (drop 1 ys)
            shiftPorts (_:xs) ys = shiftPorts xs ys

cup_,lolly_,twist_,diamond_ :: Int -> Diagram
cup_ n = Diagram (replicate (n+2) ' ') [(n,Cup)]
lolly_ n = Diagram (replicate n ' ') [(n,Lolly ' ')]
twist_ n = Diagram (replicate (n+2) ' ') [(n,Twist)]
diamond_ n = Diagram (replicate (n+1) ' ') [(n,Diamond ' ')]


cup :: Int -> Char -> Char -> Diagram
lolly :: Int -> Char -> Diagram
twist :: Int -> Char -> Char -> Diagram
diamond :: Int -> Char -> Char -> Diagram

cup n a b = Diagram (replicate n ' ' ++ [b,a]) [(n,Cup)]
lolly n a = Diagram (replicate n ' ') [(n,Lolly a)]
twist n a b = Diagram (replicate n ' ' ++ [b,a]) [(n,Twist)]
diamond n a b = Diagram (replicate n ' ' ++ [a]) [(n,Diamond b)]


pretty :: Diagram -> Diagram
pretty Diagram{..} = 
    Diagram {
        inPorts = reverse $ take (length inPorts) ['a'..]
      , d = d
    }

updateInPorts :: Int -> Generator -> [Char] -> [Char]
updateInPorts n Twist xs = take n xs ++ [b,a] ++ drop (n+2) xs
    where
        a = xs !! n
        b = xs !! (n+1)
updateInPorts n (Diamond b) xs =  take n xs ++ [b] ++ drop (n+1) xs
updateInPorts n Cup xs =  take (n+1) xs ++ drop (n+2) xs
updateInPorts n (Lolly b) xs =  take n xs ++ [b] ++ drop n xs
