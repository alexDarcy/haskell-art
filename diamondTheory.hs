{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
-- import Data.List
import Data.List.Split
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

-- Bug : the enveloppe do not take the linewidth (lw) so we set it to none
triangleLeft = triangleRect # rotateBy (1/2) # fc white # lc white # lw none

triangleRight = triangleRect # fc black #lc black # lw none

-- Color of the right triangle is the inverse of the left triangle, which is not
-- modifiable.
-- We enforce old behaviour for the origin of the tile: we want the point of
-- tangency, enforced by "align"
smallTile = beside (r2 (1,-1)) (triangleLeft # align (r2 (1, -1)))
                                (triangleRight)

-- Place a list of 4 object into a matrix
-- The origin must be placed at the center with align
createMatrix x = matrix # alignX 0 # alignY 0 
  where matrix = (x !! 0 ||| x !! 1 ) 
                         ===
                 (x !! 2 ||| x !! 3) 

smallTile' :: Int -> Diagram B R2
smallTile' x = smallTile # rotate x'
  where x' = (fromIntegral x)*pi/2 @@ rad

mediumTile angles = createMatrix listTiles
  where 
    listTiles = map smallTile' angles

-- Beware reflectX is actually a reflection in respect to the Y axis, so the
-- naming convention is inverted
-- Needs a list of 16 angles
largeTile :: [Int] -> Bool -> Bool -> Diagram B R2
largeTile angles xSymmetry ySymmetry = createMatrix [a, b, c, d]
  where 
    a = mediumTile $ chunks !! 0
    b = if (ySymmetry) then a # reflectX else mediumTile $ chunks !! 1
    c = if (xSymmetry) then a # reflectY else mediumTile $ chunks !! 2
    d 
      | ySymmetry && xSymmetry = a # rotateBy (-1/2)
      | ySymmetry  = c # reflectX
      | xSymmetry  = b # reflectY
      | otherwise = mediumTile $ chunks !! 3
    chunks = chunksOf 4 angles


-- Needs a list of 16 angles
largeTile' :: ([Int], Int) -> Diagram B R2
largeTile' x = largeTile n xSymmetry ySymmetry
  where 
    n = fst x
    axis = snd x
    xSymmetry = axis == 1 || axis == 3
    ySymmetry = axis == 2 || axis == 3

centerPos x = (x-0.5)*4 + (x-1)*d
    where d = 0.5

diamondTheory = position (zip (map p2 pos) (map largeTile' $ zip angles axes))
  where 
    nb = 3
    pos = [(centerPos x, centerPos y) | x <- [1..nb], y <- [1..nb]]
    angles = take (nb*nb) $ chunksOf 16 $ randInts 3
    axes = take (nb*nb) $ randInts 7


main = mainWith $ diamondTheory
