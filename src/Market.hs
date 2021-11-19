module Market (
    Market(valueDate),
    Priceable(price),
    Discount,
    Forward,
    VolSurface,
    marketFactory,
    findVol,
    findEquityForward,
    findDiscount ) where

import qualified Data.Map as Map
import Dates
import Discount
import Forward
import Volatility

-- public data types
type VolSurface = ([Date] -> [Fwd]) -> Date -> Strike -> Vol
type Forward = [Date] -> [Fwd]
type Discount = [Date] -> [Df]
-- private data types
type RateCurve = [Date] -> [Rate]

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
    spots :: Map.Map String Double,
    dividends :: Map.Map String [Dividend],
    yieldCurves :: Map.Map String RateCurve,
    carryCurves :: Map.Map String RateCurve,
    volSurfaces :: Map.Map String VolSurface }

-- |Find a discount curve, given the name of the credit entity or currency
findDiscount :: Market -> String -> Either String Discount
findDiscount market ccy = do
    rates <- find ccy (yieldCurves market) "yield curve"
    let dt = act365 (valueDate market)
    return $ df (logDf dt rates)

-- |Find an equity forward curve, given the name of the underlying and the
-- |name of the credit entity or currency of the underlying.
findEquityForward :: Market -> String -> String -> Either String Forward
findEquityForward market ul ccy = do
    rates <- find ccy (yieldCurves market) "yield curve"
    carry <- find ul (carryCurves market) "carry curve"
    spot <- find ul (spots market) "spot"
    divs <- find ul (dividends market) "dividends"
    let dt = act365 (valueDate market)
    return $ equityFwd dt rates carry spot divs

-- |Find a vol surface, given the name of the underlying
findVol :: Market -> String -> Either String VolSurface
findVol market key = find key (volSurfaces market) "vol surface"

-- |Finds a piece of market data, returning an error if not found
find :: String -> Map.Map String a -> String -> Either String a 
find key m msg = case Map.lookup key m of
    Just x -> Right x
    Nothing -> Left ("Failed to find " ++ msg ++ " for " ++ key)

-- |Factory for a Market, given some spots, divs, rate and carry curves and vol smiles
marketFactory :: Date -> [(String, Spot)] -> [(String, [Dividend])] -> [(String, RateCurve)] -> [(String, RateCurve)] -> [(String, VolSurface)] -> Market
marketFactory date spots divs rates carries vols = Market date s d r c v where
    s = Map.fromList spots
    d = Map.fromList divs
    r = Map.fromList rates
    c = Map.fromList carries
    v = Map.fromList vols

-- |Interface to price an instrument given a market
class Priceable a where
    -- |Try to price the given instrument given a market.
    price :: a -> Market -> Either String Double
