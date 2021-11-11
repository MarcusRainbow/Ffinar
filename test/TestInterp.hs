module TestInterp (
    interpTests,
    today,
    sampleRates,
    assertApprox,
    assertApproxList) where

import Test.HUnit
import Interp
import Dates

interpTests :: [Test]
interpTests = [
    testInterpPillar,
    testInterpsPillar,
    testInterpFirstPillar,
    testInterpLastPillar,
    testInterpsFirstLastPillar,
    testInterp,
    testInterps,
    testInterpMap]

today :: Date
today = dateFromISO 20211110

sampleRates :: [Point]
sampleRates = [
    (today,                0.005),
    (today `add_days` 5,   0.0055),
    (today `add_days` 10,  0.0062),
    (today `add_days` 30,  0.008),
    (today `add_days` 90,  0.011),
    (today `add_days` 123, 0.015),
    (today `add_days` 187, 0.017),
    (today `add_days` 203, 0.0175),    
    (today `add_days` 365, 0.0174),    
    (today `add_days` 730, 0.0172),
    (today `add_days` 930, 0.0171)] 

testInterpPillar :: Test
testInterpPillar = 
    TestCase $ assertEqual "At one year, the rate is 0.0174"
    0.0174 (interp sampleRates (dateFromISO 20221110))

testInterpsPillar :: Test
testInterpsPillar = 
    TestCase $ assertEqual "At one/two years, the rates are [0.0174, 0.0172]"
    [0.0174, 0.0172] (interps sampleRates [(dateFromISO 20221110), (dateFromISO 20231110)])

testInterpFirstPillar :: Test
testInterpFirstPillar = 
    TestCase $ assertEqual "Today, the rate is 0.005"
    0.005 (interp sampleRates today)

testInterpLastPillar :: Test
testInterpLastPillar = 
    TestCase $ assertEqual "On the last pillar, the rate is 0.0171"
    0.0171 (interp sampleRates (today `add_days` 930))

testInterpsFirstLastPillar :: Test
testInterpsFirstLastPillar = 
    TestCase $ assertEqual "On the first/last pillar, the rates are 0.005, 0.0171"
    [0.005, 0.0171] (interps sampleRates [today, (today `add_days` 930)])

testInterp :: Test
testInterp = 
    TestCase $ assertApprox "At 18 months, the rate is 0.0173"
    1e-6 0.0173 (interp sampleRates (dateFromISO 20230510))

testInterps :: Test
testInterps = 
    TestCase $ assertApproxList "testing many points on the rate curve"
    1e-6 [0.0173, 0.0172504, 0.0172, 0.0171695]
    (interps sampleRates [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

-- |Same test as above, but using interp and map
testInterpMap :: Test
testInterpMap = 
    TestCase $ assertApproxList "testing many points on the rate curve one at a time"
    1e-6 [0.0173, 0.0172504, 0.0172, 0.0171695]
    (map (interp sampleRates) [
        (dateFromISO 20230510), 
        (dateFromISO 20230810),
        (dateFromISO 20231110), 
        (dateFromISO 20240110)])

assertApprox :: String -> Double -> Double -> Double -> Assertion
assertApprox preface tolerance expected actual =
    if approx tolerance expected actual then
        assertBool preface True -- always succeeds
    else
        assertEqual preface expected actual

approx :: Double -> Double -> Double -> Bool
approx tolerance expected actual = abs (actual - expected) < tolerance

assertApproxList :: String -> Double -> [Double] -> [Double] -> Assertion
assertApproxList preface tolerance expected actual =
    if all (\(e, a) -> approx tolerance e a) (zip expected actual) then
        assertBool preface True -- always succeeds
    else
        assertEqual preface expected actual
