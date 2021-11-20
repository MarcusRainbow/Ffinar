-- module Spec
-- where

import TestDates
import TestInterp
import TestDiscount
import TestForward
import TestVolatility
import TestMarket
import TestEuropean
import TestRisks
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList (
    datesTests ++
    interpTests ++ 
    discountTests ++
    forwardTests ++ 
    volTests ++ 
    marketTests ++ 
    europeanTests ++
    riskTests)
