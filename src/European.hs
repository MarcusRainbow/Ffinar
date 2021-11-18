module European (
    European(European),
    PutCall(PUT, CALL),
    black76Call,
    black76Put) where

import Dates
import Market
import Statistics.Distribution
import Statistics.Distribution.Normal

-- |Enum defining a put (option to sell) or call (option to buy)
data PutCall = PUT | CALL deriving (Eq, Show, Read)

-- -- |Normal distribution with mean = variance = 1
-- unitNormal :: Distribution
-- unitNormal = nor

-- |The 1976 formulation of the Black-Scholes formula for pricing a European
-- |call option. Parameters discount d, forward f, variance v, strike k, 
black76Call :: Double -> Double -> Double -> Double -> Double
black76Call d f v k = d * ((n d1) * f - (n d2) * k) where
    d1 = (log (f/k) + 0.5 * v) / sqrt v
    d2 = d1 - v
    n = cumulative standard

-- |Put call parity says that C - P = S - K, or that here
-- |P = C + d (k - f)
black76Put :: Double -> Double -> Double -> Double -> Double
black76Put d f v k = (black76Call d f v k) + d * (k - f)

-- |European option
data European = European {
    putOrCall :: PutCall,
    expiry :: Date,
    creditEntity :: String,   -- e.g. Currency for a risk-free option
    underlying :: String,
    strike :: Double } deriving (Eq, Show, Read)

-- |Price a European given a discount, forward and vol surface
priceGivenCurves :: European -> Date -> Discount -> Forward -> VolSurface -> Double
priceGivenCurves e valueDate d f v = 
    let
        k = (strike e)
        date = (expiry e)
        dates = [date]   -- only one date of interest (ignoring settlement)
        discount = head (d dates)
        forward = head (f dates)
        vol = v date k
        t = act365 valueDate date
        variance = vol^2 * t
    in
        case putOrCall e of
            PUT -> black76Put discount forward variance k
            CALL -> black76Call discount forward variance k

-- |Generic pricer for European, where it finds the data it needs in the market
instance Priceable European where
    price e m = do
        let ul = (underlying e)
        let valDate = valueDate m
        discount <- findDiscount m (creditEntity e)
        forward <- findForward m ul
        volSurface <- findVol m ul
        return $ priceGivenCurves e valDate discount forward volSurface
