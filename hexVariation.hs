{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Numeric

-- Note : the side of a hexagon is the radius of its circumscribed circle

-- Apothem (distance from the center to the side)
h = sqrt(3)/2

-- Small height
h' = cos(pi/3)

hexagon' :: Diagram B R2
hexagon' = mconcat [arc1 # translateX (-1)
                  , vrule (2*h)
                  , arc1 # rotateBy (1/2) # translateX 1 
                  , polygon (with & polyType .~ PolyRegular 6 1)
                  ]
    where
      arc1 = arc' 0.5 (-pi/3 @@ rad) (pi/3 @@ rad) 

rotateHexagon' n = hexagon' # rotate (2*n*pi/3 @@ rad)

centerPosition :: Int -> Int -> (Double, Double)
centerPosition x y 
  | (x `mod` 2 == 0) = ((2-h')*x', 2*y'*h) 
  | otherwise        = ((2-h')*x', (2*y'-1)*h)
  where 
    x' = fromIntegral x
    y' = fromIntegral y

hexVariation :: StdGen -> Diagram B R2
hexVariation g = position (zip (map p2 pos) (map rotateHexagon' test))
--hexVariation g = position (zip (map p2 pos) (repeat hexa ))
  where 
    pos = [(centerPosition x y) | x <- [0..10], y <- [0..2]]
    test = take 20 $ randomRs (0, 2) g
    hexa = hexagon'

main = do
  g <- newStdGen
  mainWith $ hexVariation g
