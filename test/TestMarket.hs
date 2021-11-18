module TestMarket(
    marketTests,
    sampleMarket) where

import Test.HUnit
import Discount
import Dates
import Forward
import Volatility
import Market
import Interp
import TestInterp
import TestDiscount
import TestForward
import TestVolatility

marketTests :: [Test]
marketTests = [
    testFindDiscount,
    testFindForward,
    testFindVol,
    testFailDiscount,
    testFailForward,
    testFailVol]

sampleMarket :: Market
sampleMarket = marketFactory today d f v where
    d = [("GBP", sampleYieldCurve)]
    f = [("GSK.L", sampleEquityForward)]
    v = [("GSK.L", sampleVol)]

testFindDiscount :: Test
testFindDiscount = 
    TestCase $ assertApprox "Find discount and match value"
    1e-6 (exp (-2.588e-2)) (case findDiscount sampleMarket "GBP" of
        Left msg -> error msg
        Right discount -> head $ discount [dateFromISO 20230510])

testFindForward :: Test
testFindForward = 
    TestCase $ assertApprox "Find forward and match value"
    1e-12 109.49213655596257 (case findForward sampleMarket "GSK.L" of
        Left msg -> error msg
        Right forward -> head $ forward [dateFromISO 20240510])

testFindVol :: Test
testFindVol = 
    TestCase $ assertApprox "Find vol and match value"
    1e-12 1.0636857688602352 (case findVol sampleMarket "GSK.L" of
        Left msg -> error msg
        Right vol -> vol (today `add_days` 50) 90)

testFailDiscount :: Test
testFailDiscount = 
    TestCase $ assertEqual "Fail to find discount"
    "Failed to find discount for GSK.L" (
        case findDiscount sampleMarket "GSK.L" of
            Left msg -> msg
            Right _ -> "found discount")
    
testFailForward :: Test
testFailForward = 
    TestCase $ assertEqual "Should fail to find forward"
    "Failed to find forward for gsk.l" (
        case findForward sampleMarket "gsk.l" of
            Left msg -> msg
            Right _ -> "found forward")
    
testFailVol :: Test
testFailVol = 
    TestCase $ assertEqual "Should fail to find vol"
    "Failed to find vol surface for GSK.L+" (
        case findVol sampleMarket "GSK.L+" of
            Left msg -> msg
            Right _ -> "found vol")
        