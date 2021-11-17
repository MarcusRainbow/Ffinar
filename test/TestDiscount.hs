module TestDiscount(
    discountTests, 
    sampleRateFunction,
    sampleYieldCurve) where

import Test.HUnit
import Interp
import Discount
import Dates
import TestInterp

discountTests :: [Test]
discountTests = [
    testYieldPillars,
    testYields,
    testDfs,
    testBumpedYields]

sampleRateFunction :: [Date] -> [Rate]
sampleRateFunction = interps sampleRates

sampleYieldCurve :: [Date] -> [LogDf]
sampleYieldCurve = logDf (act365 today) sampleRateFunction

sampleBumpedYieldCurve :: [Date] -> [LogDf]
sampleBumpedYieldCurve = 
    logDf (act365 today) (flatBumpRate sampleRateFunction 0.0001)

testYieldPillars :: Test
testYieldPillars = 
    TestCase $ assertApproxList "At one/two years, the log_dfs are 2"
    1e-12 [(-0.0174), (-2) * 0.0172] (sampleYieldCurve 
        [dateFromISO 20221110, dateFromISO 20231110])

testYields :: Test
testYields = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [-2.588e-2,-3.01528e-2,-3.44e-2,-3.7208e-2]
    (sampleYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

testDfs :: Test
testDfs = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [exp (-2.588e-2), exp (-3.01528e-2), exp (-3.44e-2), exp (-3.7208e-2)]
    (df sampleYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

testBumpedYields :: Test
testBumpedYields = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [-2.6029722649652844e-2,-3.032756764871458e-2,-3.46e-2,-3.742513561643836e-2]
    (sampleBumpedYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])
