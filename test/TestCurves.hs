module TestCurves (curvesTests) where

import Test.HUnit
import Curves
import Dates
import TestInterp

curvesTests :: [Test]
curvesTests = []

sampleYieldCurve :: YieldCurve
sampleYieldCurve = YieldCurve (today, sampleRates)
