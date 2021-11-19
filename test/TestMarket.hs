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
sampleMarket = marketFactory today s d r c v where
    s = [("GSK.L", sampleSpot)]
    d = [("GSK.L", sampleDivs)]
    r = [("GBP", sampleRateFunction)]
    c = [("GSK.L", sampleCostOfCarryFunction)]
    v = [("GSK.L", sampleVolSurface)]

testFindDiscount :: Test
testFindDiscount = 
    TestCase $ assertApprox "Find discount and match value"
    1e-6 (exp (-2.588e-2)) (case findDiscount sampleMarket "GBP" of
        Left msg -> error msg
        Right discount -> head $ discount [dateFromISO 20230510])

testFindForward :: Test
testFindForward = 
    TestCase $ assertApprox "Find forward and match value"
    1e-12 109.49213655596257 (case findEquityForward sampleMarket "GSK.L" "GBP" of
        Left msg -> error msg
        Right forward -> head $ forward [dateFromISO 20240510])

testFindVol :: Test
testFindVol = 
    TestCase $ assertApprox "Find vol and match value"
    1e-12 1.0636857688602352 (case findVol sampleMarket "GSK.L" of
        Left msg -> error msg
        Right vol -> vol sampleEquityForward (today `add_days` 50) 90)

testFailDiscount :: Test
testFailDiscount = 
    TestCase $ assertEqual "Should fail to find discount"
    "Failed to find yield curve for GSK.L" (
        case findDiscount sampleMarket "GSK.L" of
            Left msg -> msg
            Right _ -> "found discount")
    
testFailForward :: Test
testFailForward = 
    TestCase $ assertEqual "Should fail to find forward"
    "Failed to find carry curve for gsk.l" (
        case findEquityForward sampleMarket "gsk.l" "GBP" of
            Left msg -> msg
            Right _ -> "found forward")
    
testFailVol :: Test
testFailVol = 
    TestCase $ assertEqual "Should fail to find vol"
    "Failed to find vol surface for GSK.L+" (
        case findVol sampleMarket "GSK.L+" of
            Left msg -> msg
            Right _ -> "found vol")
        