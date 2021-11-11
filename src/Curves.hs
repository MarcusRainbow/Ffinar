module Curves (
    Discount,
    YieldCurve (YieldCurve)) where

import Dates
import Interp

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
data YieldCurve = YieldCurve (Date, [Point])

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
