module Instrument (
    Priceable(price),
    Pricer) where

import Market

-- |A function that returns a price (or error) given a market
type Pricer = Market -> Either String Double

-- |Interface to return a pricer function given an instrument
class Priceable a where
    price :: a -> Pricer

