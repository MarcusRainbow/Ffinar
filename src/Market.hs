module Market (
    Market(valueDate),
    Priceable(price),
    Discount,
    Forward,
    VolSurface,
    marketFactory,
    findVol,
    findForward,
    findDiscount ) where

import qualified Data.Map as Map
import Dates
import Discount
import Forward
import Volatility

type VolSurface = Date -> Strike -> Vol
type Forward = [Date] -> [Fwd]
type Discount = [Date] -> [Df]

-- |The Market contains all the market data needed to price everything.
data Market = Market {
    valueDate :: Date,
    discountCurves :: Map.Map String Discount,
    forwards :: Map.Map String Forward,
    volSurfaces :: Map.Map String VolSurface }

-- |Find a discount curve, given the name of the credit entity or currency
findDiscount :: Market -> String -> Either String Discount
findDiscount market key = find key (discountCurves market) "discount"

-- |Find a forward curve, given the name of the underlying
findForward :: Market -> String -> Either String Forward
findForward market key = find key (forwards market) "forward"

-- |Find a vol surface, given the name of the underlying
findVol :: Market -> String -> Either String VolSurface
findVol market key = find key (volSurfaces market) "vol surface"

-- |Finds a piece of market data, returning an error if not found
find :: String -> Map.Map String a -> String -> Either String a 
find key m msg = case Map.lookup key m of
    Just x -> Right x
    Nothing -> Left ("Failed to find " ++ msg ++ " for " ++ key)

-- |Factory for a Market, given some discounts, forwards and vols
marketFactory :: Date -> [(String, Discount)] -> [(String, Forward)] -> [(String, VolSurface)] -> Market
marketFactory date discounts forwards vols = Market date d f v where
    d = Map.fromList discounts
    f = Map.fromList forwards
    v = Map.fromList vols

-- |Interface to price an instrument given a market
class Priceable a where
    -- |Try to price the given instrument given a market.
    price :: a -> Market -> Either String Double
