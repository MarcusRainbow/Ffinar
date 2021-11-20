module Risks (
    deltaGamma,
    pv01,
    vegaVolga) where

import Dates
import Market
import Instrument

-- |A bump takes a bumpsize and applied to a market returns a bumped market
type Bump = BumpSize -> Market -> Market

-- |Calculate the symmetric delta and gamma of any instrument in a market 
-- |to the given underlying with the given relative bumpsize. May fail and
-- |return an error string.
deltaGamma :: Id -> BumpSize -> Pricer -> Market -> Either String (Double, Double)
deltaGamma ul relBump p m = do
    spot <- findSpot m ul
    dg <- symmetricRisks (bumpSpot ul) (spot * relBump) p m
    return dg

-- |Calculate the symmetric vega and volga of any instrument in a market
-- |to the given underlying with the given absolute bumpsize. May fail and
-- |return an error string.
vegaVolga :: Id -> BumpSize -> Pricer -> Market -> Either String (Double, Double)
vegaVolga ul = symmetricRisks (bumpVol ul)

-- |Calculate the asymmetric PV01 of any instrument in a market
-- |to the given entity name. Always uses a 1bp bump.
pv01 :: Id -> Pricer -> Market -> Either String Double
pv01 ccy = asymmetricRisk (bumpRate ccy) 1e-4

-- |Calculate a first-order and second-order bumped differential, based on
-- |the given bump method
symmetricRisks :: Bump -> BumpSize -> Pricer -> Market -> Either String (Double, Double)
symmetricRisks bump bumpSize price market = do
    let up = bump bumpSize market
    let dn = bump (-bumpSize) market
    upPrice <- price up
    midPrice <- price market -- recalculates many times, but beta reduction should optimise
    dnPrice <- price dn
    let delta = (upPrice - dnPrice) / (2 * bumpSize)
    let gamma = (upPrice + dnPrice - 2 * midPrice) / bumpSize^2
    return (delta, gamma)

-- |Calculate a first-order bumped differential, based on the given bump method
asymmetricRisk :: Bump -> BumpSize -> Pricer -> Market -> Either String Double
asymmetricRisk bump bumpSize price market = do
    let up = bump bumpSize market
    upPrice <- price up
    basePrice <- price market
    let delta = (upPrice - basePrice) / bumpSize
    return delta
