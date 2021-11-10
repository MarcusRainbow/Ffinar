module TestDates (
    testYMDFromDateEpoch,
    testDateFromYMDEpoch,
    testDateFromISOEpoch,
    testYMDFromDateToday,
    testDateFromYMDToday,
    testDateFromISOToday)
where

import Test.HUnit
import Dates

testYMDFromDateEpoch :: Test
testYMDFromDateEpoch = 
    TestCase $ assertEqual "date 0 in modified Julian should be 17 Nov 1858"
    (1858, 11, 17) (ymd (dateFromMJD 0))

testDateFromYMDEpoch :: Test
testDateFromYMDEpoch = 
    TestCase $ assertEqual "17 Nov 1858 should be date 0 in modified Julian"
    (dateFromMJD 0) (dateFromYMD (1858, 11, 17))

testDateFromISOEpoch :: Test
testDateFromISOEpoch = 
    TestCase $ assertEqual "ISO 18581117 should be date 0 in modified Julian"
    (dateFromMJD 0) (dateFromISO 18581117)

testYMDFromDateToday :: Test
testYMDFromDateToday = 
    TestCase $ assertEqual "date 59528 in modified Julian should be 10 Nov 2021"
    (2021, 11, 10) (ymd (dateFromMJD 59528))

testDateFromYMDToday :: Test
testDateFromYMDToday = 
    TestCase $ assertEqual "17 Nov 1858 should be date 59528 in modified Julian"
    (dateFromMJD 59528) (dateFromYMD (2021, 11, 10))

testDateFromISOToday :: Test
testDateFromISOToday = 
    TestCase $ assertEqual "ISO 20211110 should be date 59528 in modified Julian"
    (dateFromMJD 59528) (dateFromISO 20211110)
