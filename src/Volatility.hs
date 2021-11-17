module Volatility (
    Vol,
    Smile,
    Strike,
    SIV(SIV),
    siv,
    sivFactory,
    vol) where

import Dates
import Interp
import Discount
import Forward

-- |A volatility is the square root of the variance divided by the time in years.
type Vol = Double

-- |The strike of a vanilla option. It defines the volatility that is
-- |used to value the option.
type Strike = Double

-- |A vol smile is a function that takes a strike and gives a vol
type Smile = Strike -> Vol

-- |Returns a volatility given an expiry date and a time. Pass in a list
-- |of volatility smiles and a forward. It linear interpolates in variance
-- |along the forward.
vol :: (Date -> Time) -> [(Date, Smile)] -> ([Date] -> [Fwd]) -> Date -> Smile
vol ft ss ff d = case bracket (\(d', _) -> d' `compare` d) ss of
    LT_EXT _      -> error "Cannot extrapolate to the left"
    RT_EXT _      -> error "Cannot extrapolate to the right"
    PILLAR (_, s) -> s  -- on a pillar date, just return the smile
    INTERP l r    -> lerpSmile ft ff l r d

-- |Linear interpolate in variance an entire smile. We interpolate along the
-- |forward, using strikes scaled by sqrt(t)
lerpSmile :: (Date -> Time) -> ([Date] -> [Fwd]) -> (Date, Smile) -> (Date, Smile) -> Date -> Smile
lerpSmile ft ff (ld, ls) (rd, rs) d =
    let
        -- find the three forwards we need
        [lf, f, rf] = ff [ld, d, rd]
        -- and the three times
        lt = ft ld
        t = ft d
        rt = ft rd
        -- and hence the strike multipliers at left and right
        lm = sqrt (lt / t)
        rm = sqrt (rt / t)
        -- allows us to create a volatility interpolator for any strike
        smile k = 
            let
                lk = lf * ((k / f) ** lm)
                rk = rf * ((k / f) ** rm)
                lv = (ls lk)^2 * lt
                rv = (rs rk)^2 * rt
                v = lerp lv rv t    -- interpolate the variance
            in
                (sqrt v) / t
    in
        smile

-- |The SIV parameterisation was devised by Merrill Lynch in 1999 and
-- |is well-described by Jim Gatheral https://arxiv.org/pdf/1204.0646.pdf.
-- |The parameters in order are a, b, rho, m, sigma.
data SIV = SIV (Double, Double, Double, Double, Double)

-- |The SIV parameterisation also depends on the strike
siv :: SIV -> Strike -> Vol
siv (SIV (a, b, rho, m, sigma)) k =
    a + b * (rho * (k - m) + sqrt ((k - m)^2 + sigma^2))

-- |A utility to return a list of (Date, Smile), given a list of 
-- |(Date, SIV).
sivFactory :: [(Date, SIV)] -> [(Date, Smile)]
sivFactory smiles =
    map (\(d, s) -> (d, siv s)) smiles
