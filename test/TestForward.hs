module TestForward(forwardTests) where

import Test.HUnit
import Discount
import Dates
import Forward
import Interp
import TestInterp
import TestDiscount

forwardTests :: [Test]
forwardTests = [
    testForwards,
    testEquityForwardsNoDivs,
    testEquityForwards]

sampleCostOfCarry :: [Point]
sampleCostOfCarry = [
    (today,                0.002),
    (today `add_days` 89,  0.002),
    (today `add_days` 90,  0.011),
    (today `add_days` 93,  0.002),
    (today `add_days` 202, 0.002),
    (today `add_days` 203, 0.012),
    (today `add_days` 207, 0.002),
    (today `add_days` 930, 0.002)] 

sampleCostOfCarryFunction :: [Date] -> [Rate]
sampleCostOfCarryFunction = interps sampleCostOfCarry

sampleSpot :: Spot
sampleSpot = 120

sampleDivs :: [Dividend]
sampleDivs = [
    Dividend (dateFromISO 20230510) (dateFromISO 20230513) 5,
    Dividend (dateFromISO 20231110) (dateFromISO 20231113) 7,
    Dividend (dateFromISO 20240510) (dateFromISO 20240513) 3]

sampleForward :: [Date] -> [Fwd]
sampleForward = fwd (dtAct365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot

sampleEquityForwardNoDivs :: [Date] -> [Fwd]
sampleEquityForwardNoDivs = 
    equityFwd (dtAct365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot []

sampleEquityForward :: [Date] -> [Fwd]
sampleEquityForward = 
    equityFwd (dtAct365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot sampleDivs

testForwards :: Test
testForwards = 
    TestCase $ assertApproxList "At six month intervals, the forwards are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        122.77827612802493, 123.70401578573441, 124.61681455483054] 
    (sampleForward [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510])

testEquityForwardsNoDivs :: Test
testEquityForwardsNoDivs = 
    TestCase $ assertApproxList "At six month intervals, equity forwards no divs are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        122.77827612802493, 123.70401578573441, 124.61681455483054] 
    (sampleEquityForwardNoDivs [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510])

testEquityForwards :: Test
testEquityForwards = 
    TestCase $ assertApproxList "At six month intervals, equity forwards are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        117.77889252838303, 111.66779056388835, 109.49213655596257] 
    (sampleEquityForward [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510])
