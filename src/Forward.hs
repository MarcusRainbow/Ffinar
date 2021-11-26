module Forward (
    Fwd, Spot, 
    Dividend,
    cashDiv, mixDiv,
    fwd, equityFwd) where

import Utils
import Dates
import Interp
import Discount
import Debug.Trace

-- |A forward is the expected value of some asset at some time in the future.
type Fwd = Double

-- |A cash forward is the expected cash value of the asset, which is generally
-- |considered to be non-stochastic.
type CashFwd = Double

-- |The spot is the expected value of some asset today. (Actually, there may
-- |be subtleties to do with settlement.)
type Spot = Double

-- |A cost of carry is subtracted from a rate
type CostOfCarry = Double

-- |A dividend is defined as a cash amount plus a relative amount. It is
-- |removed from the value of the forward at the ex date and pays on the
-- |pay date.
-- |
-- |The relative amount is a multiple of the forward value at the relDate.
-- |Typically this is well before the ex date, as the cash amount is
-- |announced anything up to a year beforehand, and it depends on the
-- |stock price some time before that.
data Dividend = Dividend {
    exDate :: Date,
    payDate :: Date,
    relDate :: Date,
    cash :: Double,
    rel :: Double } deriving (Show)

-- |Creates a purely cash dividend
cashDiv :: Date -> Date -> Double -> Dividend
cashDiv exDate payDate cash = 
    Dividend exDate payDate zeroDate cash 0.0

-- |Creates a cash/relative dividend
mixDiv :: Date -> Date -> Date -> Double -> Double -> Dividend
mixDiv exDate payDate relDate cash rel = 
    Dividend exDate payDate relDate cash rel

-- |The expected value of an asset on a given date, given the date-to-time 
-- |convention, yield curve and cost of carry.
fwd :: (Date -> Time) -> ([Date] -> [Rate]) -> ([Date] -> [CostOfCarry]) ->
    Spot -> [Date] -> [Fwd]
fwd ft fr fq spot ds = map fwd' (zip3 (fr ds) (fq ds) ds) where
    fwd' (r, q, d) = let f = spot * exp ((r - q) * (ft d)) in
        -- trace ("fwd r=" ++ show r ++ " q=" ++ show q ++ " d=" ++ show d ++ " =" ++ show f)
        f

-- |Calculates an equity forward, which is calculated like any standard
-- |forward but also subtracting the value of dividends.
equityFwd ::  (Date -> Time) -> ([Date] -> [Rate]) -> ([Date] -> [CostOfCarry]) ->
    Spot -> [Dividend] -> [Date] -> [Fwd]
equityFwd ft fr fq spot divs ds =
    let
        -- forwards ignoring divs div ex/pay/rel dates assuming spot = 1
        -- note that the pay and rel dates are not necessarily sorted in date order
        fwd' = fwd ft fr fq 1
        rough x d1 d2 = (d1 `sub` d2) > x -- how much out of order are pay/rel dates?
        payFwds = sortedMap (rough 5) fwd' (map payDate divs)
        relFwds = sortedMap (rough 30) fwd' (map relDate divs)
        
        -- accumulate dividend NPVs then use them to create a flatRight interpolator
        npvDivs = foldlr npvDiv 0 [] (zip3 divs payFwds relFwds) where
            npvDiv div l r = (l + valueDiv div l):r
            valueDiv (div, payFwd, relFwd) npvDivs' =
                ((cash div) + (rel div) * relFwd * (spot - npvDivs')) / payFwd
        exDates = map exDate divs

        -- forwards and divnpv on requested dates assuming spot = 1
        outFwds = fwd' ds
        outDivs = flatRights 0 (zip exDates npvDivs) ds
    in
        -- now we can calculate the equity forward on the requested dates 
        map (\(f, d) -> f * (spot - d)) (zip outFwds outDivs)
