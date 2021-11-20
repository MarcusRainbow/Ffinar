module TestRisks(
    riskTests) where

import Test.HUnit
import Discount
import Dates
import Forward
import Volatility
import Market
import Instrument
import European
import Risks
import Interp
import TestInterp
import TestDiscount
import TestForward
import TestVolatility
import TestMarket
import TestEuropean

riskTests :: [Test]
riskTests = [
    testEuropeanCallDelta,
    testEuropeanPutDelta,
    testEuropeanCallPV01,
    testEuropeanPutPV01,
    testEuropeanCallVega,
    testEuropeanPutVega]

sampleDeltaGamma :: Pricer -> Market -> Either String (Double, Double)
sampleDeltaGamma = deltaGamma "GSK.L" 0.01

sampleVegaVolga :: Pricer -> Market -> Either String (Double, Double)
sampleVegaVolga = vegaVolga "GSK.L" 0.01

samplePV01 :: Pricer -> Market -> Either String Double
samplePV01 = pv01 "GBP"

testEuropeanCallDelta :: Test
testEuropeanCallDelta = TestCase $ do
    let (delta, gamma) = errorIfLeft $ sampleDeltaGamma (price sampleEuropeanCall) sampleMarket
    assertApprox "Delta of European call" 1e-6 0.7412611699871349 delta
    assertApprox "Gamma of European call" 1e-6 1.6231325742543934e-2 gamma

testEuropeanPutDelta :: Test
testEuropeanPutDelta = TestCase $ do
    let (delta, gamma) = errorIfLeft $ sampleDeltaGamma (price sampleEuropeanPut) sampleMarket
    assertApprox "Delta of European put" 1e-5 (-0.25674082868020015) delta  -- roughly call delta - 1
    assertApprox "Gamma of European put" 1e-6 1.6231325742543934e-2 gamma

testEuropeanCallVega :: Test
testEuropeanCallVega = TestCase $ do
    let (vega, volga) = errorIfLeft $ sampleVegaVolga (price sampleEuropeanCall) sampleMarket
    assertApprox "Vega of European call" 1e-6 32.676087861058576 vega
    assertApprox "Volga of European call" 1e-6 92.57663123539928 volga

testEuropeanPutVega :: Test
testEuropeanPutVega = TestCase $ do
    let (vega, volga) = errorIfLeft $ sampleVegaVolga (price sampleEuropeanPut) sampleMarket
    assertApprox "Vega of European put" 1e-6 32.676087861058576 vega
    assertApprox "Volga of European put" 1e-6 92.57663123539928 volga

testEuropeanCallPV01 :: Test
testEuropeanCallPV01 = TestCase $ do
    let pv01 = errorIfLeft $ samplePV01 (price sampleEuropeanCall) sampleMarket
    assertApprox "PV01 of European call" 1e-6 29.40439105896253 pv01

testEuropeanPutPV01 :: Test
testEuropeanPutPV01 = TestCase $ do
    let pv01 = errorIfLeft $ samplePV01 (price sampleEuropeanPut) sampleMarket
    assertApprox "PV01 of European put" 1e-6 (-88.5197733309262) pv01

-- |Convert Either <error> <value> to <value>, throwing if error
errorIfLeft :: Either String b -> b
errorIfLeft x = case x of
    Left msg -> error msg
    Right x' -> x'
