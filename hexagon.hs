{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Numeric

hexagon' :: Diagram B R2
hexagon' = mconcat [arc1 # translateX (-1)
                  , vrule 2 
                  ,  arc1 # rotateBy (1/2) # translateX 1 
                  ]
    where
      arc1 = arc (-pi/2 @@ rad) (pi/2 @@ rad)

rotateHexagon' n = hexagon' # rotate (120*n @@ deg)

example :: StdGen -> Diagram B R2
example g = position (zip (map p2 locs) (map rotateHexagon' test))
  where 
    locs = [(x, y) | x <- [2,6..10], y <- [2,6..10]]
    test = take 9 $ randomRs (0, 2) g
    --locs = [(x, y) | x <- take 3 $ randomRs (0, 10) g, y <- [0, 4, 8]]


main = do
  g <- newStdGen
  mainWith $ example g
