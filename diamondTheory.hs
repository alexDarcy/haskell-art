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

-- colorNumbers :: IO [Int]
-- colorNumbers = do
--   g <- newStdGen
--   let l = randomRs (0, 1) g
--   return l

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
tile :: Colour Double -> Diagram B R2
tile color = beside (r2 (1,-1)) (triangleLeft color # align (r2 (1, -1)))
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

tile' :: (Int, Int) -> Diagram B R2
tile' x = tile color # rotate (a'*pi/2 @@ rad)
  where 
    color = generateColor $ fst x
    a' = fromIntegral $ snd x

tiles = map tile' $ zip colors angles
  where 
    colors = [1..16]
    angles = [1..16]

diamondTheory :: Diagram B R2
diamondTheory = position ( zip (map p2 (zip x y)) tiles) 
  where
    x = concat $ common
    y = concat . transpose $ common
    common = take 4 $ repeat [1..4]

main = mainWith $ diamondTheory
