-- module Spec
-- where

import TestDates
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [
    testYMDFromDateEpoch,
    testDateFromYMDEpoch,
    testDateFromISOEpoch,
    testYMDFromDateToday,
    testDateFromYMDToday,
    testDateFromISOToday]
