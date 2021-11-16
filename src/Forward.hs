module Forward (
    Fwd, Spot, 
    Dividend (Dividend),
    fwd, equityFwd) where

import Dates
import Interp
import Discount

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

-- |A dividend is defined as a cash amount plus a fraction of the
-- |forward. It is removed from the value of the forward at the ex date
-- |and pays on the pay date. (For now, we just handle the cash amount.)
data Dividend = Dividend {
    exDate :: Date,
    payDate :: Date,
    cash :: Double }

-- |The expected value of an asset on a given date, given the date-to-time 
-- |convention, yield curve and cost of carry.
-- |
-- |The methodology here ignores dividends and settlement.
fwd :: (Date -> Time) -> ([Date] -> [Rate]) -> ([Date] -> [CostOfCarry]) ->
    Spot -> [Date] -> [Fwd]
fwd ft fr fq spot ds = map fwd' (zip3 (fr ds) (fq ds) ds) where
    fwd' (r, q, d) = spot * exp ((r - q) * (ft d))

-- |Calculates an equity forward, which is calculated like any standard
-- |forward but also subtracting the value of dividends.
equityFwd ::  (Date -> Time) -> ([Date] -> [Rate]) -> ([Date] -> [CostOfCarry]) ->
    Spot -> [Dividend] -> [Date] -> [Fwd]
equityFwd ft fr fq spot divs ds =
    let
        -- forwards ignoring divs on output dates and div ex/pay dates assuming spot = 1 
        outFwds = fwd ft fr fq 1 ds
        payFwds = fwd ft fr fq 1 (map payDate divs)
        divPVs = map (valueDiv) (zip divs payFwds) where
            valueDiv (div, payFwd) = (cash div) / payFwd
    in
        equityFwdGivenDivs spot (zip divPVs divs) (zip outFwds ds)

-- |Calculates a list of equity forwards given the spot, a list of dividends
-- |and their PVs, and a list of the unadjusted forwards (with spot == 1) and their dates
equityFwdGivenDivs :: Spot -> [(Double, Dividend)] -> [(Fwd, Date)] -> [Fwd]
equityFwdGivenDivs _ _ [] = []
equityFwdGivenDivs s [] fs = map (\(f, d) -> f * s) fs  -- if no dividends, just times by spot
equityFwdGivenDivs s divs@((x,div):divs') fs@((f,d):fs') = 
    if (exDate div) > d then
        -- first div is not relevant. Output this forward, then recurse
        (f * s) : equityFwdGivenDivs s divs fs'
    else
        -- first div is before the next date to output. Adjust spot and continue
        equityFwdGivenDivs (s - x) divs' fs
