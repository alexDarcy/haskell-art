{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Numeric
import qualified Debug.Trace as D

-- ! This is a transcription in Haskell of "Hex Variation" by William Kolmyjec.
-- The algorithm itself is inspired from the version by Steve Berrick.
-- More details on the Recode project:
-- http://recodeproject.com/artwork/v3n4hex-variation


-- Note : the side of a hexagon is the radius of its circumscribed circle

-- Apothem (distance from the center to the side)
h = sqrt(3)/2

-- Small height
h' = cos(pi/3)

hexagon' :: Diagram B R2
hexagon' = mconcat [arc1 # translateX (-1)
                  , vrule (2*h)
                  , arc1 # rotateBy (1/2) # translateX 1 
--                  , hexagon 1
                  ]
    where
      arc1 = arc' 0.5 (-pi/3 @@ rad) (pi/3 @@ rad) 

rotateHexagon' :: Int -> Diagram B R2
rotateHexagon' n = hexagon' # rotate (n'*2*pi/3 @@ rad)
  where
    n' = fromIntegral n

centerPosition :: Int -> Int -> (Double, Double)
centerPosition x y 
  | (x `mod` 2 == 0) = ((2-h')*x', 2*y'*h) 
  | otherwise        = ((2-h')*x', (2*y'-1)*h)
  where 
    x' = fromIntegral x
    y' = fromIntegral y

generateAngles :: StdGen -> [Int]
generateAngles g = randomRs (0, 2) g

hexVariation :: Int -> StdGen -> Diagram B R2
hexVariation nb g = position (zip (map p2 pos) (map rotateHexagon' test))
  where 
    pos = [(centerPosition x y) | x <- [0..nb], y <- [0..nb]]
    test = take ((nb+1)*(nb+1)) $ generateAngles g
    hexa = hexagon'

main = do
  g <- newStdGen
  mainWith $ hexVariation 20 g
