module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen i1 Stop = i1
andThen Stop i1 = i1
andThen (Move x i1) i2 = Move x (andThen i1 i2)
andThen (Turn x i1) i2 = Turn x (andThen i1 i2)
andThen (SetStyle x i1) i2 = SetStyle x (andThen i1 i2)
andThen (SetColour x i1) i2 = SetColour x (andThen i1 i2)
andThen (PenDown i1) i2 = PenDown (andThen i1 i2)
andThen (PenUp i1) i2 = PenUp (andThen i1 i2)


loop :: Int -> Instructions -> Instructions
loop n i1
    | n <= 0 = Stop
    | otherwise = andThen i1 (loop (n-1) i1)

invisibly :: Instructions -> Instructions
invisibly i =  andThen (PenUp Stop) (invisiblyHelper i (PenDown Stop))

invisiblyHelper :: Instructions -> Instructions -> Instructions
invisiblyHelper (Move x i1) i2 = Move x (invisiblyHelper i1 i2)
invisiblyHelper (Turn x i1) i2 = Turn x (invisiblyHelper i1 i2)
invisiblyHelper (SetStyle x i1) i2 = SetStyle x (invisiblyHelper i1 i2)
invisiblyHelper (SetColour x i1) i2 = SetColour x (invisiblyHelper i1 i2)
invisiblyHelper (PenDown i1) i2 = PenUp (invisiblyHelper i1 (PenDown Stop))
invisiblyHelper (PenUp i1) i2 = PenUp (invisiblyHelper i1 (PenUp Stop))
invisiblyHelper Stop i2 = i2

retrace :: Instructions -> Instructions
retrace i =  retraceHelper i white (Solid 1) True (SetColour (white) (SetStyle (Solid 1) (PenDown Stop)))

retraceHelper :: Instructions -> Colour -> LineStyle -> Bool -> Instructions -> Instructions
retraceHelper (Move x i1) c s p i2
  | p = retraceHelper i1 c s p (PenDown (SetStyle s (SetColour c (Move (-1*x) i2))))
  | otherwise = retraceHelper i1 c s p (PenUp (SetStyle s (SetColour c (Move (-1*x) i2))))
retraceHelper (Turn x i1) c s p i2 = retraceHelper i1 c s p (Turn (-1*x) i2)
retraceHelper (SetStyle x i1) c s p i2 = retraceHelper i1 c x p i2
retraceHelper (SetColour x i1) c s p i2 = retraceHelper i1 x s p i2
retraceHelper (PenDown i1) c s p i2 = retraceHelper i1 c s True i2
retraceHelper (PenUp i1) c s p i2 = retraceHelper i1 c s False i2
retraceHelper Stop c s p i2 = i2

overlay :: [Instructions] -> Instructions
overlay [] = (SetColour (white) (SetStyle (Solid 1) (PenDown Stop)))
overlay (i:is) = andThen i (andThen (invisibly(retrace i)) (overlay is)) 

