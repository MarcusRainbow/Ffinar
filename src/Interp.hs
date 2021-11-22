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
interp xs d = fst (interp' xs d)

-- |Same as interp, but taking a list of dates and returning a list
-- |of values. If there are many dates looked up, this gives considerable
-- |efficiency gains (O(n) rather than O(n^2)).
-- |
-- |The dates must be monotonic increasing, though duplicates are allowed.
interps :: [Point] -> [Date] -> [Double]
interps _ [] = []
interps xs (d:ds) = let (v, xs') = interp' xs d in
    v : interps xs' ds

-- |Rolling interpolator. Interpolates the given date in the set of points,
-- |then returns the result together with a new set of points, excluding any
-- |unused to the left.
interp' :: [Point] -> Date -> (Double, [Point])
interp' xs d = case bracket (\(d', _) -> d' `compare` d) xs of
    LT_EXT xs'            -> ((leftExtrap xs' d), xs') 
    RT_EXT xs'            -> ((rightExtrap xs' d), xs')
    PILLAR xs'@((_, v):_) -> (v, xs')
    INTERP xs'@(l:r:_)    -> ((lerpPoints l r d), xs')

-- |Handle left extrapolation. Currently we just error unless it
-- |is zero date, in which case we return zero
leftExtrap :: [Point] -> Date -> Double
leftExtrap _ d = if isZeroDate d then 0 else error "Cannot extrapolate to the left"

-- |Handle right extrapolation. Currently we just error
rightExtrap :: [Point] -> Date -> Double
rightExtrap _ _ = error "Cannot extrapolate to the right"

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
-- |right, and exact match on a pillar, or interpolation. In each case, the
-- |remainder of the pillars are returned, so the head is used for most of
-- |these, or the first two items in the case of INTERP.
data Bracket a = LT_EXT [a] | RT_EXT [a] | PILLAR [a] | INTERP [a]

-- |Try to finds a pair of values that bracket the given value
bracket :: (a -> Ordering) -> [a] -> Bracket a
bracket pred [] = error "Cannot bracket an empty list"
bracket pred xs@(x:xs') = case pred x of
    LT -> bracket_l pred x xs' -- keep looking to the right
    EQ -> PILLAR xs            -- exact match
    GT -> LT_EXT xs            -- extrapolate left

bracket_l :: (a -> Ordering) -> a -> [a] -> Bracket a
bracket_l _ x [] = RT_EXT [x]  -- extrapolate right
bracket_l pred xl xs@(x:xs') = case pred x of
    LT -> bracket_l pred x xs' -- keep looking to the right
    EQ -> PILLAR xs            -- exact match
    GT -> INTERP (xl:xs)       -- found the bracket, so interpolate

