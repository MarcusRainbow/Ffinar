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
        -- forwards ignoring divs on output dates and div ex/pay dates assuming spot = 1
        -- also collect forwards excluding divs on rel dates.
        outFwds = fwd ft fr fq 1 ds
        payFwds = fwd ft fr fq 1 (map payDate divs)
        relFwds = fwd ft fr fq 1 (map relDate divs)
        
        -- cumulative dividend PVs
        npvDivs = foldlr npvDiv 0 [] (zip3 divs payFwds relFwds) where
            npvDiv div l r = (l + valueDiv div l):r
            valueDiv (div, payFwd, relFwd) npvDivs' =
                ((cash div) + (rel div) * relFwd * (spot - npvDivs')) / payFwd
    in
        equityFwdGivenDivs spot 0 (zip npvDivs divs) (zip outFwds ds)

-- |Calculates a list of equity forwards given the spot, a list of dividends
-- |and their NPVs, and a list of the unadjusted forwards (with spot == 1) and their dates
equityFwdGivenDivs :: Spot -> Double -> [(Double, Dividend)] -> [(Fwd, Date)] -> [Fwd]
equityFwdGivenDivs _ _ _ [] = []
equityFwdGivenDivs spot npv [] fs = map (\(f, _) -> f * (spot - npv)) fs -- no more divs
equityFwdGivenDivs spot npv divs@((x,div):divs') fs@((f,d):fs') =
    if (exDate div) > d then
        -- first div is in the future. Output this forward, then recurse
        (f * (spot - npv)) : equityFwdGivenDivs spot npv divs fs'
    else
        -- first div is before the next date to output. Replace npv and continue
        equityFwdGivenDivs spot x divs' fs
