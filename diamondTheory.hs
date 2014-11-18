{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
--import Data.Colour
-- import qualified Debug.Trace as D

side = sqrt(2)
triangleRect :: Diagram B R2
triangleRect = polygon ( with
  & polyType .~ PolySides
     [ 135 @@ deg, 90 @@ deg]
     [ 1        , side      ]
  )

colorNumbers :: [Int]
colorNumbers = randomRs (0, 1) (mkStdGen 42)

generateColor :: (Ord a, Num a, Floating a) => Int -> Colour a
generateColor nb 
  | (nb == 0) = black
  | otherwise = white


triangleLeft = triangleRect # rotateBy (1/2) # fc red # lc red
triangleRight = triangleRect # fc black

twoTriangles = beside (r2 (1,-1)) (triangleLeft) (triangleRight)

oneLine:: Diagram B R2
oneLine = cat (r2 (1, 0)) $ take 4 $ repeat twoTriangles

diamondTheory :: Diagram B R2
diamondTheory = cat (r2 (0, 1)) $ take 4 $ repeat oneLine

main = mainWith $ diamondTheory
