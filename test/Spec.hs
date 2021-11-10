-- module Spec
-- where

import TestDates
import TestCurves
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList (datesTests ++ curvesTests)
