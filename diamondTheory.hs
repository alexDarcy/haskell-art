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

colorNumbers :: Int -> [Int]
colorNumbers seed = randomRs (0, 1) (mkStdGen seed)

-- colorNumbers :: IO [Int]
-- colorNumbers = do
--   g <- newStdGen
--   let l = randomRs (0, 1) g
--   return l

generateColor :: Int -> Colour Double
generateColor nb 
  | (nb == 0) = black
  | otherwise = white


coloursPairs :: [Colour Double] -> [(Colour Double, Colour Double)]
coloursPairs [] = []
coloursPairs (x:[]) = []
coloursPairs (x:y:xs) = (x,y):coloursPairs xs

triangleLeft :: Colour Double -> Diagram B R2
triangleLeft color = triangleRect # rotateBy (1/2) # fc color # lc color

triangleRight :: Colour Double -> Diagram B R2
triangleRight color = triangleRect # fc color #lc color

tile :: (Colour Double, Colour Double) -> Diagram B R2
tile color = beside (r2 (1,-1)) (triangleLeft c1) (triangleRight c2)
  where 
    c1 = fst color
    c2 = snd color

oneLine:: Int -> Diagram B R2
oneLine seed = cat (r2 (1, 0)) $ map tile colors
  where 
    colors = coloursPairs $ map generateColor $ take 8 $ colorNumbers seed

diamondTheory :: Diagram B R2
diamondTheory = cat (r2 (0, 1)) $ map oneLine seeds
  where seeds = take 4 $ randomRs (0, 1) (mkStdGen 38)

main = mainWith $ diamondTheory
