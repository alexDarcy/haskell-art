{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
--import Diagrams.TwoD.Arc 

example :: Diagram B R2
example = position (zip (map p2 locs) (repeat twoArcs))
--example = cat (r2 (1, 1)) (map twoArcs [1..3])
  where 
    locs = [(x, y) | x <- [0, 4, 8], y <- [0, 4, 8]]
    --twoArcs n = regPoly 5 1
    twoArcs = mconcat [arc1 
                        , vrule 2 # translateX 2
                        ,  arc1 # rotateBy (1/2) # translateX 4 
                        ]
    arc1 = arc (-pi/2 @@ rad) (pi/2 @@ rad)


main = mainWith example
