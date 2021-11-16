module TestCurves (curvesTests) where

import Test.HUnit
import Curves
import Dates
import TestInterp

curvesTests :: [Test]
curvesTests = [
    testYieldPillar,
    testYieldPillars,
    testYield,
    testYields,
    testDf,
    testDfs,
    testBumpedYield,
    testBumpedYields]

sampleYieldCurve :: YieldCurve
sampleYieldCurve = YieldCurve (today, sampleRates)

sampleBumpedYieldCurve :: FlatBumpedDiscount YieldCurve
sampleBumpedYieldCurve = FlatBumpedDiscount (sampleYieldCurve, 0.0001)

testYieldPillar :: Test
testYieldPillar = 
    TestCase $ assertApprox "At one year, the log_df is -0.0174"
    1e-12 (-0.0174) (log_df sampleYieldCurve (dateFromISO 20221110))

testYieldPillars :: Test
testYieldPillars = 
    TestCase $ assertApproxList "At one/two years, the log_dfs are 2"
    1e-12 [(-0.0174), (-2) * 0.0172] (log_dfs sampleYieldCurve 
        [dateFromISO 20221110, dateFromISO 20231110])

testYield :: Test
testYield = 
    TestCase $ assertApprox "At 18 months, the log_df is -0.02588"
    1e-6 (-2.588e-2) (log_df sampleYieldCurve (dateFromISO 20230510))

testYields :: Test
testYields = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [-2.588e-2,-3.01528e-2,-3.44e-2,-3.7208e-2]
    (log_dfs sampleYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

testDf :: Test
testDf = 
    TestCase $ assertApprox "At 18 months, the log_df is -0.02588"
    1e-6 (exp (-2.588e-2)) (df sampleYieldCurve (dateFromISO 20230510))

testDfs :: Test
testDfs = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [exp (-2.588e-2), exp (-3.01528e-2), exp (-3.44e-2), exp (-3.7208e-2)]
    (dfs sampleYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

testBumpedYield :: Test
testBumpedYield = 
    TestCase $ assertApprox "At 18 months, the log_df is -0.02588 + 1bp"
    1e-6 (-2.588e-2 - 1.5e-4) (log_df sampleBumpedYieldCurve (dateFromISO 20230510))

testBumpedYields :: Test
testBumpedYields = 
    TestCase $ assertApproxList "testing many points on the yield curve"
    1e-6 [-2.6029722649652844e-2,-3.032756764871458e-2,-3.46e-2,-3.742513561643836e-2]
    (log_dfs sampleBumpedYieldCurve [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])
