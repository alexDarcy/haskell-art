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

generateColor :: Int -> Colour Double
generateColor nb 
  | (nb == 0) = black
  | otherwise = white

-- Bug : the enveloppe do not take the linewidth (lw) so we set it to none
triangleLeft :: Colour Double -> Diagram B R2
triangleLeft color = triangleRect # rotateBy (1/2) # fc color # lc color # lw none

triangleRight :: Colour Double -> Diagram B R2
triangleRight color = triangleRect # fc color #lc color # lw none

-- Color of the right triangle is the inverse of the left triangle
-- We enforce old behaviour for the origin of the tile: we want the point of
-- tangency, enforced by "align"
square' :: Colour Double -> Diagram B R2
square' color = beside (r2 (1,-1)) (triangleLeft color # align (r2 (1, -1)))
                                (triangleRight color')
  where color' = if color == white then black else white

-- rotateTile :: (Diagram B R2, Int) -> Diagram B R2
-- --rotateTile x | D.trace ("rotatetile" ++ show (fromIntegral $ snd x)) False = undefined
-- rotateTile x = fst x # rotate (x'*pi/2 @@ rad) 
--   where x' = fromIntegral $ snd x
-- 
-- lineTiles :: Int -> Diagram B R2
-- lineTiles seed = cat (r2 (1, 0)) $ tiles'
--   where 
--     tiles' = map rotateTile $ zip (take 4 $ repeat tile) angles
--     angles = take 4 $ randomRs (1, 4) (mkStdGen seed)
-- 
-- largeTile seed = vcat $ map lineTiles seeds
--   where seeds = take 4 $ randoms (mkStdGen seed)
-- 
-- lineLargeTiles seed = hcat' (with & sep .~ 1) $ map largeTile seeds
--   where seeds = take 4 $ randoms (mkStdGen seed)
-- 
-- setLines seed = vcat' (with & sep .~ 1) $ map lineLargeTiles seeds
--   where seeds = take 4 $ randoms (mkStdGen seed)

listColors = map generateColor $ randomRs (0, 1) (mkStdGen 13)

listAngles = map (\x -> x*pi/2 @@ rad) $ repeat 0  --randomRs (0, 4) (mkStdGen 13) 

smallTile :: (Colour Double, Angle) -> Diagram B R2
smallTile x = square' (fst x) # rotate (snd x)

-- tiles = map tile' $ zip (listColors True) listAngles
--     
-- diamondTheory :: Diagram B R2
-- diamondTheory = position ( zip (map p2 (zip x y)) tiles) 
--   where
--     x = concat $ common
--     y = concat . transpose $ common
--     common = take 2 $ repeat [1..2]

-- Place a list of 4 object into a matrix
-- The origin must be placed at the center with align
createMatrix x = matrix # alignX 0 # alignY 0 
  where matrix = (x !! 0 ||| x !! 1 ) 
                         ===
                 (x !! 2 ||| x !! 3) 


mediumTile = createMatrix listTiles
  where 
    listTiles = map smallTile $ zip c a
    c = take 4 listColors 
    a = take 4 listAngles

largeTile xSymmetry ySymmetry = createMatrix [a, b, c, d]
  where 
    a = mediumTile
    b = if (ySymmetry) then a # reflectAbout p vY else mediumTile
    c = if (xSymmetry) then a # reflectAbout p vX  else mediumTile
    d = if (ySymmetry) 
        then 
          if (xSymmetry) then a # rotateBy (-1/2) else a # reflectAbout p vY 
        else mediumTile
    p = p2 (1, 0)
    vY = r2 (0, 1)
    vX = r2 (1, 0)

diamondTheory :: Diagram B R2
diamondTheory = largeTile True True

main = mainWith $ diamondTheory
