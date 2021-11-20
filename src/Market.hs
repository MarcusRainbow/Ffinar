module Market (
    Market(valueDate),
    Discount,
    Forward,
    VolSurface,
    Id,
    BumpSize,
    marketFactory,
    findVol,
    findSpot,
    findEquityForward,
    findDiscount,
    bumpSpot,
    bumpRate,
    bumpVol ) where

import qualified Data.Map as Map
import Dates
import Discount
import Forward
import Volatility

type VolSurface = ([Date] -> [Fwd]) -> Date -> Strike -> Vol
type Forward = [Date] -> [Fwd]
type Discount = [Date] -> [Df]
type RateCurve = [Date] -> [Rate]
type BumpSize = Double  -- absolute size of bump
type Id = String        -- identifier such as credit entity or underlying

-- |The Market contains all the market data needed to price everything.
-- |In general, we store partially processed data, thus RateCurve rather
-- |than a list of points. The key point here is that we need to store
-- |the lowest level that allows the kinds of risk bumps we want to use.
-- |RateCurve is better than Discount, because a rate bump such as PV01
-- |bumps the rate rather than the discount factors. We could implement
-- |a PV01 bump on top of a discount factor, but it would mean taking
-- |the log, recalculating the time to expiry, bumping and exponentiating.
data Market = Market {
    valueDate :: Date,
    spots :: Map.Map Id Double,
    dividends :: Map.Map Id [Dividend],
    yieldCurves :: Map.Map Id RateCurve,
    carryCurves :: Map.Map Id RateCurve,
    volSurfaces :: Map.Map Id VolSurface }

-- |Find a discount curve, given the name of the credit entity or currency
findDiscount :: Market -> Id -> Either String Discount
findDiscount market ccy = do
    rates <- find ccy (yieldCurves market) "yield curve"
    let dt = act365 (valueDate market)
    return $ df (logDf dt rates)

-- |Find a spot, given the name of the underlying.
findSpot :: Market -> Id -> Either String Spot
findSpot market ul = find ul (spots market) "spot"

-- |Find an equity forward curve, given the name of the underlying and the
-- |name of the credit entity or currency of the underlying.
findEquityForward :: Market -> Id -> Id -> Either String Forward
findEquityForward market ul ccy = do
    rates <- find ccy (yieldCurves market) "yield curve"
    carry <- find ul (carryCurves market) "carry curve"
    spot <- find ul (spots market) "spot"
    divs <- find ul (dividends market) "dividends"
    let dt = act365 (valueDate market)
    return $ equityFwd dt rates carry spot divs

-- |Find a vol surface, given the name of the underlying
findVol :: Market -> Id -> Either String VolSurface
findVol market key = find key (volSurfaces market) "vol surface"

-- |Finds a piece of market data, returning an error if not found
find :: Id -> Map.Map Id a -> String -> Either String a 
find key m msg = case Map.lookup key m of
    Just x -> Right x
    Nothing -> Left ("Failed to find " ++ msg ++ " for " ++ key)

-- |Factory for a Market, given some spots, divs, rate and carry curves and vol smiles
marketFactory :: Date -> [(Id, Spot)] -> [(Id, [Dividend])] -> [(Id, RateCurve)] -> [(Id, RateCurve)] -> [(Id, VolSurface)] -> Market
marketFactory date spots divs rates carries vols = Market date s d r c v where
    s = Map.fromList spots
    d = Map.fromList divs
    r = Map.fromList rates
    c = Map.fromList carries
    v = Map.fromList vols

-- |Bump the spot by an absolute amount. This bump uses the default
-- |dynamics for the vol surface -- in other words leaving it alone.
bumpSpot :: Id -> BumpSize -> Market -> Market
bumpSpot ul bump m = m { spots = spots' } where
    spots' = Map.adjust (\s -> s + bump) ul (spots m)

-- |Bump the rate by an absolute amount, e.g. PV01
bumpRate :: Id -> BumpSize -> Market -> Market
bumpRate ccy bump m = m { yieldCurves = yieldCurves' } where
    yieldCurves' = Map.adjust (\r -> flatBumpRate r bump) ccy (yieldCurves m)

-- |Bump the volatility by an absolute amount.
bumpVol :: Id -> BumpSize -> Market -> Market
bumpVol ul bump m = m { volSurfaces = volSurfaces' } where
    volSurfaces' = Map.adjust (\v -> flatBumpVol v bump) ul (volSurfaces m)
