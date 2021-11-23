module TestForward(
    forwardTests,
    sampleEquityForward,
    sampleCostOfCarryFunction,
    sampleSpot,
    sampleDivs) where

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
    (today `add_days` 2000, 0.002)] 

sampleCostOfCarryFunction :: [Date] -> [Rate]
sampleCostOfCarryFunction = interps sampleCostOfCarry

sampleSpot :: Spot
sampleSpot = 120

sampleDivs :: [Dividend]
sampleDivs = let d = dateFromISO in [
    cashDiv (d 20230510) (d 20230513) 5,
    cashDiv (d 20231110) (d 20231113) 7,
    cashDiv (d 20240510) (d 20240513) 3,
    mixDiv  (d 20241110) (d 20241113) (d 20230410) 4 0.01,
    mixDiv  (d 20250510) (d 20250513) (d 20231010) 3 0.01,
    mixDiv  (d 20251110) (d 20251113) (d 20240410) 2 0.015,
    mixDiv  (d 20260510) (d 20260513) (d 20241010) 2 0.015,
    mixDiv  (d 20261110) (d 20261113) (d 20250410) 1 0.02,
    mixDiv  (d 20270510) (d 20270513) (d 20251010) 1 0.02]

sampleForward :: [Date] -> [Fwd]
sampleForward = fwd (act365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot

sampleEquityForwardNoDivs :: [Date] -> [Fwd]
sampleEquityForwardNoDivs = 
    equityFwd (act365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot []

sampleEquityForward :: [Date] -> [Fwd]
sampleEquityForward = 
    equityFwd (act365 today) sampleRateFunction sampleCostOfCarryFunction sampleSpot sampleDivs

testForwards :: Test
testForwards = 
    TestCase $ assertApproxList "At six month intervals, the forwards are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        122.77827612802493, 123.70401578573441, 124.61681455483054,
        125.56035108019694, 126.49560918968065, 127.45131799121688,
        128.39631734767855, 129.36193735739084]
    (sampleForward [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510,
        dateFromISO 20241110, dateFromISO 20250510,
        dateFromISO 20251110, dateFromISO 20260510,
        dateFromISO 20261110])

testEquityForwardsNoDivs :: Test
testEquityForwardsNoDivs = 
    TestCase $ assertApproxList "At six month intervals, equity forwards no divs are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        122.77827612802493, 123.70401578573441, 124.61681455483054,
        125.56035108019694, 126.49560918968065, 127.45131799121688,
        128.39631734767855, 129.36193735739084] 
    (sampleEquityForwardNoDivs [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510,
        dateFromISO 20241110, dateFromISO 20250510,
        dateFromISO 20251110, dateFromISO 20260510,
        dateFromISO 20261110])

testEquityForwards :: Test
testEquityForwards = 
    TestCase $ assertApproxList "At six month intervals, equity forwards are ..."
    1e-12 [
        120.0, 120.88469041993034, 121.86230292737272,
        117.77889252838303, 111.66779056388835, 109.49213655596257,
        105.24434509322552, 101.99319200421841, 99.25884910851306,
        96.53030965652835, 94.35694060289615] 
    (sampleEquityForward [
        dateFromISO 20211110, dateFromISO 20220510,
        dateFromISO 20221110, dateFromISO 20230510,
        dateFromISO 20231110, dateFromISO 20240510,
        dateFromISO 20241110, dateFromISO 20250510,
        dateFromISO 20251110, dateFromISO 20260510,
        dateFromISO 20261110])
