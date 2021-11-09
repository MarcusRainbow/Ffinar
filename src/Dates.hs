module Dates (
    Date, 
    add_days, 
    sub,
    dateFromMJD,
    dateFromISO,
    dateFromYMD,
    ymd,
    act365) where

import Numeric
import Text.Read
import qualified Text.Read.Lex as L
import Text.Printf

-- |In ffin-ar, dates are represented by an integer containing a modified Julian.
-- |Dates begin at market open in Tokyo and end at market close in Chicago. It
-- |is therefore possible to ignore issues of time, daylight-saving and time zone.
-- |For discounting, forward contracts and most other issues, time of day is not
-- |important, as settlement is always at a conventional time. For volatility-
-- |dependent products such as options time of day can be relevant, so additional
-- |information may be required.
-- |
-- |We do not define all arithmetic operations on Date, as many make no sense,
-- |but it does make sense to add an integer to a date, or subtract two dates
-- |to give an integer. These do not match the signatures in Num, so we
-- |use named operators.
data Date = Date Int deriving (Ord, Eq, Bounded)

-- |Always show dates in short ISO format (yyyymmdd)
instance Show Date where 
  show x = printf "%4d%2d%2d" y m d where (y, m, d) = ymd x

-- |Always read in dates from short ISO format
instance Read Date where
  readPrec     = readDate convertDate
  readListPrec = readListPrecDefault
  readList     = readListDefault

readDate :: (L.Lexeme -> ReadPrec Date) -> ReadPrec Date
readDate convert =
    parens
    (   do 
            x <- lexP
            convert x
    )

convertDate :: L.Lexeme -> ReadPrec Date
convertDate (L.Number n)
    | Just i <- L.numberToInteger n = return (dateFromISO i)
convertDate _ = pfail  

-- |Converts a modified julian number into a date
dateFromMJD :: Int -> Date
dateFromMJD mjd = Date mjd

-- |Converts an ISO date string represented as a 6 digit int
-- |into a Date.
dateFromISO :: Integral a => a -> Date
dateFromISO i = dateFromYMD (y, m, d) where
    y = fromIntegral (i `div` 10000)
    m = fromIntegral ((i `div` 100) `mod` 100)
    d = fromIntegral (i `mod` 100)

-- |Converts year month day into a Date. (Formula from wikipedia
-- |https://en.wikipedia.org/wiki/Julian_day)
dateFromYMD :: (Int, Int, Int) -> Date
dateFromYMD (y, m, d) = Date mjd where
    mjd =
        (1461 * (y + 4800 + (m - 14) `div` 12)) `div` 4 +
        (367 * (m - 2 - 12 * ((m - 14) `div` 12))) `div` 12 -
        (3 * ((y + 4900 + (m - 14) `div` 12) `div` 100)) `div` 4 +
        d - 32075 - 2400000

-- |Converts a date (modified Julian) to year, month and date
-- |using Edward Graham Richards' algorithm. Works for dates within
-- |the Gregorian calendar (proleptic, where applicable).
ymd :: Date -> (Int, Int, Int)
ymd (Date d) = 
    let
        y = 4716
        v = 3
        j = 1401
        u = 5
        m = 2
        s = 153
        n = 12
        w = 2
        r = 4
        b = 274277
        p = 1461
        c = -38
    
        jj = d + 2400000
        f = jj + j + (((4 * jj + b) `div` 146097) * 3) `div` 4 + c
        e = r * f + v
        g = (e `mod` p) `div` r
        h = u * g + w
        dd = (h `mod` s) `div` u + 1
        mm = h `div` s + m `mod` n + 1
        yy = e `div` p - y + (n + m - mm) `div` n
    in
        (yy, mm, dd)

-- |Add a (possibly negative) offset in days to a date
add_days :: Date -> Int -> Date
add_days (Date d) i = Date (d + i)

-- |Returns the number of days difference between two days (negative if the second is bigger)
sub :: Date -> Date -> Int
sub (Date l) (Date r) = (l - r)

-- |Calculate a year-fraction represented by the given count of days, using
-- |the Act365 convention, which assumes every year has 365 days.
act365 :: Int -> Double
act365 d = (fromIntegral d) / 365
