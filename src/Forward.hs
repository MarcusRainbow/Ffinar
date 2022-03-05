module Forward (
    Fwd, Spot, 
    Dividend,
    cashDiv, mixDiv,
    fwd, equityFwd) where

import Utils
import Dates
import Interp
import Discount
import Data.Ord
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
        (payFwds, relFwds) = divFwds divs fwd'
        
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

-- |
-- Calculate the forwards needed for dividend calculation, at the relative
-- and pay dates of all the dividends. (Non-strict calculation, so not
-- all need be calculated.) This calculation is tricky because the pay dates
-- are not necessarily in exact date order, and the rel dates are considerably
-- offset, but it is most efficient to calculate all the forwards in one
-- call with all the dates in order.
divFwds :: [Dividend] -> ([Date] -> [Fwd]) -> ([Double], [Double])
divFwds divs fwd = 
    let
        -- pay dates and rel dates decorated by integers in sequence.
        -- (odd for pay dates and even for rel dates)
        payDates = zip (map payDate divs) [1,3..]
        relDates = zip (map relDate divs) [2,4..]

        -- rel dates should be in order, but not pay dates
        relApprox (d1,_) (d2,_) = (d1 `sub` d2) > 5 -- allow 5 days mismatch
        payDates' = lazySortBy (comparing fst) relApprox payDates

        -- merge both into one sorted date list and fetch the forwards
        both = mergeBy (comparing fst) relDates payDates'
        fwds = zip (fwd (map fst both)) both

        -- split back into two lists (odd for pay, even for rel)
        (relFwds, payFwds) = unmerge (even.snd.snd) fwds

        -- sort the pay fwds back into the original order
        payFwds' = lazySortBy (comparing (snd.snd)) approx payFwds where
            approx = (\(_,x1) (_,x2) -> relApprox x1 x2)
    in
        (map fst relFwds, map fst payFwds')
