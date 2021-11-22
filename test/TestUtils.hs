module TestUtils (utilsTests) where

import Test.HUnit
import Utils

utilsTests :: [Test]
utilsTests = [
    testRunningSum]

runningSum :: Num a => [a] -> [a]
runningSum xs = foldlr (\x l r -> (x + l):r) 0 [] xs

testRunningSum :: Test
testRunningSum = 
    TestCase $ assertEqual "running sum of first five integers"
    [1, 3, 6, 10, 15] (take 5 (runningSum [1..]))

