module TestEuropean(
    europeanTests,
    sampleEuropeanCall) where

import Test.HUnit
import Discount
import Dates
import Forward
import Volatility
import Market
import European
import Interp
import TestInterp
import TestDiscount
import TestForward
import TestVolatility
import TestMarket

europeanTests :: [Test]
europeanTests = [
    testBlack76Call,
    testBlack76Put,
    testZSBlack76Call,
    testEuropeanCall,
    testEuropeanPut]

sampleEuropeanCall :: European
sampleEuropeanCall = 
    European CALL (dateFromISO 20221110) "GBP" "GSK.L" 120 

sampleEuropeanPut :: European
sampleEuropeanPut = 
    European PUT (dateFromISO 20221110) "GBP" "GSK.L" 120 

testBlack76Call :: Test
testBlack76Call = 
    TestCase $ assertApprox "Find price of European call using direct call to analytics"
    1e-6 6.749815840940159 (black76Call 
    0.9827505058020646      -- discount (see DiscountTests/testDfs) 
    121.86230292737272      -- forward (see ForwardTests/testForwards)
    (0.34961879753578035^2) -- vol (see VolatilityTests/testVolInterp1y)
    120)

testBlack76Put :: Test
testBlack76Put = 
    TestCase $ assertApprox "Find price of European put using direct call to analytics"
    1e-6 4.919636697107949 (black76Put 
    0.9827505058020646 
    121.86230292737272
    (0.34961879753578035^2)
    120)

testZSBlack76Call :: Test
testZSBlack76Call = 
    TestCase $ assertApprox "Find price of European zero-strike call using direct call to analytics"
    1e-6 (0.9827505058020646 * 121.86230292737272) (black76Call 
    0.9827505058020646 
    121.86230292737272
    (0.34961879753578035^2)
    0)

testEuropeanCall :: Test
testEuropeanCall = 
    TestCase $ assertApprox "Find price of European call using price method"
    1e-6 6.749815840940159 (case price sampleEuropeanCall sampleMarket of
        Left msg -> error msg
        Right v -> v)

testEuropeanPut :: Test
testEuropeanPut = 
    TestCase $ assertApprox "Find price of European put using price method"
    1e-6 4.919636697107949 (case price sampleEuropeanPut sampleMarket of
        Left msg -> error msg
        Right v -> v)
