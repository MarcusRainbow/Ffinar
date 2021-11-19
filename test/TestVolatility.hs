module TestVolatility(
    volTests,
    sampleVolSurface,
    sampleVol) where

import Test.HUnit
import Discount
import Dates
import Forward
import Volatility
import Interp
import TestInterp
import TestDiscount
import TestForward

volTests :: [Test]
volTests = [
    testSIV,
    testVolPillar,
    testVolInterp,
    testVolInterp1y]

-- |SIV parameters a, b, rho, m, sigma
sampleSIV :: [(Date, SIV)]
sampleSIV = [
    (today,                SIV (0.10, 0.01, 0.090, 130, 0.2)),
    (today `add_days` 30,  SIV (0.12, 0.01, 0.040, 130, 0.2)),
    (today `add_days` 60,  SIV (0.15, 0.01, 0.030, 130, 0.2)),
    (today `add_days` 120, SIV (0.14, 0.01, 0.020, 130, 0.2)),
    (today `add_days` 240, SIV (0.13, 0.01, 0.010, 130, 0.2)),
    (today `add_days` 360, SIV (0.12, 0.01, 0.005, 130, 0.2)),
    (today `add_days` 720, SIV (0.11, 0.01, 0.002, 130, 0.2)),
    (today `add_days` 930, SIV (0.10, 0.01, 0.001, 130, 0.2))] 

-- |Sample vol pillars using SIV
sampleVolPillars :: [(Date, Smile)]
sampleVolPillars = sivFactory sampleSIV

sampleVolSurface :: ([Date] -> [Fwd]) -> Date -> Smile
sampleVolSurface = vol (act365 today) sampleVolPillars

sampleVol :: Date -> Smile
sampleVol = sampleVolSurface sampleEquityForward

testSIV :: Test
testSIV = 
    TestCase $ assertApproxList "In the first smile, the vols are ..."
    1e-12 [
    0.46400499996875033,0.4185057142390679,0.3730066665925943,
    0.3275079998720041,0.2820099997500125,0.23651333274079342,
    0.1910199980003999,0.14553998401278723,0.10200000000000001,
    0.15453998401278723,0.2090199980003999]
    (map (snd (head sampleVolPillars)) [90, 95 .. 140])

testVolPillar :: Test
testVolPillar = 
    TestCase $ assertApproxList "The first smile on the pillar date is ..."
    1e-12 [
    0.46400499996875033,0.4185057142390679,0.3730066665925943,
    0.3275079998720041,0.2820099997500125,0.23651333274079342,
    0.1910199980003999,0.14553998401278723,0.10200000000000001,
    0.15453998401278723,0.2090199980003999]
    (map (sampleVol today) [90, 95 .. 140])

testVolInterp :: Test
testVolInterp = 
    TestCase $ assertApproxList "Interpolating between smiles gives ..."
    1e-12 [
    1.0636857688602352,0.968329041767872,0.8735540565436402,
    0.7793344469263354,0.6856555969099993,0.5925198528556902,
    0.49995892021337723,0.4080694686846377,0.3285901686679542,
    0.3545080773679434,0.45268319929305467]
    (map (sampleVol (today `add_days` 50)) [90, 95 .. 140])

testVolInterp1y :: Test
testVolInterp1y = 
    TestCase $ assertApproxList "Interpolating between smiles at one year gives ..."
    1e-12 [
    0.8904273265696886,0.8047276923347503,0.7171835615654877,
    0.6278506104712193,0.5367803905594342,0.4440210711732766,
    0.34961879753578035,0.2536244748889143,0.1576818990949708,
    0.2525263160115035,0.35351593101219825]
    (map (sampleVol (dateFromISO 20221110)) [90, 95 .. 140])
