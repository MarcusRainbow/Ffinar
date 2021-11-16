module Discount (
    Df, LogDf, Rate, Time,
    df, logDf, flatBumpRate) where

import Dates
import Interp

-- |A discount factor is the value of a zero-coupon bond,
-- |valued as of some bae date.
type Df = Double

-- |A log discount factor is the log of a value of a zero-coupon bond
type LogDf = Double

-- |A rate is a terminal discount rate such that exp(-rt) is the
-- |value of a zero coupon bond
type Rate = Double

-- |A time is measured in years, typically Act365 but not necessarily.
-- |For example, in Brazilian markets time is Act252 counting only 
-- |business days
type Time = Double

-- |Converts dates into discount factors, given a log-df function
df :: ([Date] -> [LogDf]) -> [Date] -> [Df]
df f d = map exp (f d)

-- |Converts dates into log discount factors, given a date to time 
-- |function and a date to rate function.
logDf :: (Date -> Time) -> ([Date] -> [Rate]) -> [Date] -> [LogDf]
logDf ft fr ds = map (\(r, d) -> -r * (ft d)) (zip (fr ds) ds)

-- |Applies a flat bump to a rate function
flatBumpRate :: ([Date] -> [Rate]) -> Double -> ([Date] -> [Rate])
flatBumpRate rf bump = (\dates -> map (+bump) (rf dates))
