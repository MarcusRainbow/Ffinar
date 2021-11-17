module Interp (
    Point,
    Bracket (LT_EXT, RT_EXT, PILLAR, INTERP),
    interp,
    interps,
    bracket,
    lerpPoints,
    lerp) where

import Dates

-- |A generic curve is a list of Points with an interpolator
-- |to give points between the dates. Dates are considered to be "risk
-- |date", which means they begin at open in Tokyo and end at close in
-- |Chicago. Times of day are irrelevant, as most financial transactions
-- |are per-day.
type Point = (Date, Double)

-- |Interpolates in the given [Point] to give a value. Extrapolation
-- |always fails
interp :: [Point] -> Date -> Double
interp xs d = case bracket (\(d', _) -> d' `compare` d) xs of
    LT_EXT _      -> error "Cannot extrapolate to the left"
    RT_EXT _      -> error "Cannot extrapolate to the right"
    PILLAR (_, v) -> v
    INTERP l r    -> lerpPoints l r d

-- |Same as interp, but taking a list of dates and returning a list
-- |of values. If there are many dates looked up, this gives considerable
-- |efficiency gains (O(n) rather than O(n^2)).
-- |
-- |The dates must be monotonic increasing, though duplicates are allowed.
interps :: [Point] -> [Date] -> [Double]
interps _ [] = []
interps [] _ = error "Cannot interpolate in an empty [Point]"
interps cs@(c@(dl, vl):cs') ds@(d:ds') = case dl `compare` d of
    LT -> interps_l c cs' ds
    EQ -> vl : interps cs ds'   -- exact match to the left.
    GT -> error "Cannot extrapolate to the left"

-- |Interpolates given a left hand value and a [Point]
interps_l :: Point -> [Point] -> [Date] -> [Double]
interps_l _ _ [] = []
interps_l _ [] _ = error "Cannot extrapolate to the right"
interps_l cl@(dl, vl) cs@(cr@(dr, vr):cs') ds@(d:ds') = case dr `compare` d of
    LT -> interps_l cr cs' ds  -- keep looking
    EQ -> vr : interps_l cl cs ds'  -- exact match to the right
    GT -> lerpPoints cl cr d : interps_l cl cs ds'  -- found the bracket, so interpolate

-- |Interpolates linearly between two points on a [Point]
lerpPoints :: Point -> Point -> Date -> Double
lerpPoints (d1, v1) (d2, v2) d = 
    let x = fromIntegral (d `sub` d1) / fromIntegral (d2 `sub` d1) in
        lerp v1 v2 x

-- |Linear interpolator. This needs to be implemented carefully to avoid
-- |numerical errors. When x == l the result should be exactly a, and when
-- |x == r the result should be exactly b. For example, if we were to code
-- |this as "a + (b - a) * x" and b were very small compared with a, the
-- |term b would be ignored and the result would be zero when x == 1.
lerp :: Num a => a -> a -> a -> a
lerp a b x = a * (1 - x) + b * x

-- |The result of trying to bracket a point can be extrapolation to left or
-- |right, and exact match on a pillar, or interpolation.
data Bracket a = LT_EXT a | RT_EXT a | PILLAR a | INTERP a a

-- |Try to finds a pair of values that bracket the given value
bracket :: (a -> Ordering) -> [a] -> Bracket a
bracket pred [] = error "Cannot bracket an empty list"
bracket pred (x:xs) = case pred x of
    LT -> bracket_l pred x xs  -- keep looking to the right
    EQ -> PILLAR x             -- exact match
    GT -> LT_EXT x             -- extrapolate left

bracket_l :: (a -> Ordering) -> a -> [a] -> Bracket a
bracket_l _ x [] = RT_EXT x    -- extrapolate right
bracket_l pred xl (x:xs) = case pred x of
    LT -> bracket_l pred x xs  -- keep looking to the right
    EQ -> PILLAR x             -- exact match
    GT -> INTERP xl x          -- found the bracket, so interpolate

