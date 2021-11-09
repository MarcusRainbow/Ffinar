module TestDates (
    testDateFromYMD)
where

import Test.HUnit
import Dates

testDateFromYMD :: Test
testDateFromYMD = 
    TestCase $ assertEqual "17 Nov 1858 should be date 0 in modified Julian"
    (dateFromMJD 0) (dateFromYMD (1858, 11, 17))
