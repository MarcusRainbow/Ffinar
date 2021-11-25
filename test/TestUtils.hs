module TestUtils (utilsTests) where

import Test.HUnit
import Data.Sort
import Utils

utilsTests :: [Test]
utilsTests = [
    testRunningSum,
    testApproxSort,
    testLazySort,
    testBuffer]

runningSum :: Num a => [a] -> [a]
runningSum xs = foldlr (\x l r -> (x + l):r) 0 [] xs

testRunningSum :: Test
testRunningSum = 
    TestCase $ assertEqual "running sum of first five integers"
    [1, 3, 6, 10, 15] (take 5 (runningSum [1..]))

-- |An infinite list that is nearly sorted
sampleApproxSortedInfList :: [Int]
sampleApproxSortedInfList = map (\x -> if even x then x else x + 4) [0..]

-- |A finite list that is nearly sorted
sampleApproxSortedList :: [Int]
sampleApproxSortedList = take 100 sampleApproxSortedInfList

-- |A finite list that has been sorted
sampleSortedList :: [Int]
sampleSortedList = sort sampleApproxSortedList

testApproxSort :: Test
testApproxSort =
    TestCase $ assertEqual "Sort a finite nearly-sorted list"
    sampleSortedList
    (approxSort sampleApproxSortedList)

testLazySort :: Test
testLazySort =
    TestCase $ assertEqual "Sort an infinite nearly-sorted list"
    ([0, 2] ++ [4..101])
    (take 100 $ lazySort (\x y -> (x - y) > 5) sampleApproxSortedInfList)

testBuffer :: Test
testBuffer = 
    TestCase $ assertEqual "Buffer a partially undefined list into chunks"
    [1, 2, 3]
    (take 3 $ buffer (\x -> x == 4) [1, 2, 3, 4, 5, undefined])
