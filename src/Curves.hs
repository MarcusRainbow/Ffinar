module Curves (
    Curve,
    Discount,
    YieldCurve (YieldCurve),
    interp,
    interps) where

import Dates

-- |A generic curve is a list of (Date, Double) with an interpolator
-- |to give points between the dates. Dates are considered to be "risk
-- |date", which means they begin at open in Tokyo and end at close in
-- |Chicago. Times of day are irrelevant, as most financial transactions
-- |are per-day.
type Curve = [(Date, Double)]

-- |Interpolates in the given curve to give a value. Extrapolation
-- |always fails
interp :: Curve -> Date -> Double
interp [] _ = error "Cannot interpolate in an empty curve"
interp (c@(dl, vl):cs) d = case dl `compare` d of
    LT -> interp_l c cs d
    EQ -> vl  -- exact match to the left
    GT -> error "Cannot extrapolate to the left"

-- |Interpolates given a left hand value and a curve
interp_l :: (Date, Double) -> Curve -> Date -> Double
interp_l _ [] _ = error "Cannot extrapolate to the right"
interp_l cl@(dl, vl) (cr@(dr, vr):cs) d = case dr `compare` d of
    LT -> interp_l cr cs d  -- keep looking
    EQ -> vr  -- exact match to the right
    GT -> interp_lr cl cr d  -- found the bracket, so interpolate

-- |Interpolates linearly between two points on a curve
interp_lr :: (Date, Double) -> (Date, Double) -> Date -> Double
interp_lr (d1, v1) (d2, v2) d = 
    let x = fromIntegral (d `sub` d1) / fromIntegral (d2 `sub` d1) in
        lerp v1 v2 x

-- |Same as interp, but taking a list of dates and returning a list
-- |of values. If there are many dates looked up, this gives considerable
-- |efficiency gains (O(n) rather than O(n^2)).
-- |
-- |The dates must be monotonic increasing, though duplicates are allowed.
interps :: Curve -> [Date] -> [Double]
interps _ [] = []
interps [] _ = error "Cannot interpolate in an empty curve"
interps cs@(c@(dl, vl):cs') ds@(d:ds') = case dl `compare` d of
    LT -> interps_l c cs' ds
    EQ -> vl : interps cs ds'   -- exact match to the left.
    GT -> error "Cannot extrapolate to the left"

-- |Interpolates given a left hand value and a curve
interps_l :: (Date, Double) -> Curve -> [Date] -> [Double]
interps_l _ _ [] = []
interps_l _ [] _ = error "Cannot extrapolate to the right"
interps_l cl@(dl, vl) cs@(cr@(dr, vr):cs') ds@(d:ds') = case dr `compare` d of
    LT -> interps_l cr cs' ds  -- keep looking
    EQ -> vr : interps_l cl cs ds'  -- exact match to the right
    GT -> interp_lr cl cr d : interps_l cl cs ds'  -- found the bracket, so interpolate

-- |Linear interpolator. This needs to be implemented carefully to avoid
-- |numerical errors. When x == l the result should be exactly a, and when
-- |x == r the result should be exactly b. For example, if we were to code
-- |this as "a + (b - a) * x" and b were very small compared with a, the
-- |term b would be ignored and the result would be zero when x == 1.
lerp :: Num a => a -> a -> a -> a
lerp a b x = a * (1 - x) + b * x

-- |A generic discount curve gives the value of a zero coupon bond on
-- |a given date, relative to the base date of the curve. It is also
-- |useful to return log df, as we often need to do additional work
-- |before exponentiating, such as adding growth or subtracting a from
-- |date. It is also useful to return a list of discounts or their logs.
class Discount a where
    -- |The value of a zero-coupon on the given date.
    df :: a -> Date -> Double
    df c d = exp (log_df c d)

    -- |The natural log of the value of a zero-coupon on the given date
    log_df :: a -> Date -> Double
    log_df z d = head $ log_dfs z [d]

    -- |The value of zero-coupons on the given dates
    dfs :: a -> [Date] -> [Double]
    dfs c ds = map exp (log_dfs c ds)

    -- |The log value of zero-coupons on the given dates
    log_dfs :: a -> [Date] -> [Double]

-- |A yield curve is based on a curve of date/value pairs and a base-date
-- |and linearly interpolates between them.
data YieldCurve = YieldCurve (Date, Curve)

instance Discount YieldCurve where
    -- |log_df is (-rt) where r is interpolated from the curve and t is the time in years.
    -- |We assume Act365 time calculation and linear interpolation in rate.
    log_df (YieldCurve (base, curve)) d = (interp curve d) * act365 (base `sub` d)

    -- |Same as log_df, but operating on a list of dates. It is more efficient in that it
    -- |does not iterate through the list of yield curve pillars separately for each date
    -- |looked up. If there are as many dates looked up as pillars, this means O(n) rather
    -- |than O(n^2).
    log_dfs (YieldCurve (base, curve)) ds = 
        map (\(r, d) -> r * act365 (base `sub` d)) rds where
            rds = zip (interps curve ds) ds
