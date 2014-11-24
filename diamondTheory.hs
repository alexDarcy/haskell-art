{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Data.List
--import qualified Debug.Trace as D

side = sqrt(2)
triangleRect :: Diagram B R2
triangleRect = polygon ( with
  & polyType .~ PolySides
     [ 135 @@ deg, 90 @@ deg]
     [ 1        , side      ]
  )


colorNumbers :: Int -> [Int]
colorNumbers seed = randomRs (0, 1) (mkStdGen seed)

randInts :: Int -> [Int]
randInts seed = randomRs (0, 3) (mkStdGen seed)

listAngles :: Int -> [Angle]
listAngles seed = map helper $ randomRs (0, 3) (mkStdGen seed) 
  where
    helper :: Int -> Angle 
    helper x = (fromIntegral x)*pi/2 @@ rad

generateColor :: Int -> Colour Double
generateColor nb 
  | (nb == 0) = black
  | otherwise = white

-- Bug : the enveloppe do not take the linewidth (lw) so we set it to none
triangleLeft :: Colour Double -> Diagram B R2
triangleLeft color = triangleRect # rotateBy (1/2) # fc color # lc color # lw none

triangleRight :: Colour Double -> Diagram B R2
triangleRight color = triangleRect # fc color #lc color # lw none

-- Color of the right triangle is the inverse of the left triangle, which is not
-- modifiable.
-- We enforce old behaviour for the origin of the tile: we want the point of
-- tangency, enforced by "align"
smallTile :: Diagram B R2
smallTile = beside (r2 (1,-1)) (triangleLeft black # align (r2 (1, -1)))
                                (triangleRight white)



-- Place a list of 4 object into a matrix
-- The origin must be placed at the center with align
createMatrix x = matrix # alignX 0 # alignY 0 
  where matrix = (x !! 0 ||| x !! 1 ) 
                         ===
                 (x !! 2 ||| x !! 3) 


mediumTile seed = createMatrix listTiles
  where 
    listTiles = map (\x -> smallTile # rotate x) $  take 4 $ listAngles seed

-- Beware reflectX is actually a reflection in respect to the Y axis, so the
-- naming convention is inverted
largeTile seed xSymmetry ySymmetry = createMatrix [a, b, c, d]
  where 
    a = mediumTile seed 
    b = if (ySymmetry) then a # reflectX else mediumTile $ seed + 1
    c = if (xSymmetry) then a # reflectY else mediumTile $ seed + 2
    d 
      | ySymmetry && xSymmetry = a # rotateBy (-1/2)
      | ySymmetry  = c # reflectX
      | xSymmetry  = b # reflectY
      | otherwise = mediumTile $ seed + 3


largeTile' x = largeTile seed xSymmetry ySymmetry
  where 
    seed = fst x
    n = snd x
    xSymmetry = n == 1 || n == 3
    ySymmetry = n == 2 || n == 3


lineTiles seed =  hcat' (with & sep .~ 0.9) $ map largeTile' $ zip n seeds
  where 
    n = take 10 $ randInts seed
    seeds = take 10 $ randInts $ seed + 1

diamondTheory :: Diagram B R2
diamondTheory =  vcat' (with & sep .~ 0.9) $ map lineTiles seeds
  where
    seeds = take 10 $ randInts $ 31

main = mainWith $ diamondTheory
