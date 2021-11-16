-- module Spec
-- where

import TestDates
import TestInterp
import TestCurves
import TestDiscount
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList (datesTests ++ interpTests ++ curvesTests ++ discountTests)
