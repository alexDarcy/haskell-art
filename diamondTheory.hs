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

-- Bug : the enveloppe do not take the linewidth (lw) so we set it to none
triangleLeft :: Colour Double -> Diagram B R2
triangleLeft color = triangleRect # rotateBy (1/2) # fc color # lc color # lw none

triangleRight :: Colour Double -> Diagram B R2
triangleRight color = triangleRect # fc color #lc color # lw none

-- Color of the right triangle is the inverse of the left triangle
tile :: Colour Double -> Diagram B R2
tile color = beside (r2 (1,-1)) (triangleLeft color) (triangleRight c)
  where 
    c = if color == black then white else black

--rotateTile :: (Transformable b, V b ~ R2) => b -> Int
--rotateTile a = rotateBy (1/fromIntegral a)

oneLine:: Int -> Diagram B R2
--oneLine seed = cat (r2 (1, 0)) $ map (rotateBy (1/2)) tiles
oneLine seed = cat (r2 (1, 0)) $ tiles'
  where 
    tiles' = map (\(a,b) -> a # rotateBy (1/b)) $ zip tiles angles
    -- colors = coloursPairs $ map generateColor $ take 8 $ colorNumbers seed
    tiles = map tile colors
    colors = map generateColor $ take 4 $ colorNumbers seed
    angles = [1.0..4.0]

diamondTheory :: Diagram B R2
diamondTheory = cat (r2 (0, 1)) $ map oneLine seeds
  where seeds = take 4 $ randomRs (0, 1) (mkStdGen 13)

main = mainWith $ diamondTheory
