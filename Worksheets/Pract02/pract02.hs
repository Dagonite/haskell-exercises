-- pract02.hs

{- 1. Write a new version of your function:
absolute :: Int -> Int

from worksheet 1, this time using guards rather than an if ... then ... else. -}
absolute :: Int -> Int
absolute x
  | x > 0 = x
  | otherwise = x * (-1)

{- 2. Write a function that returns 1 for positive arguments, -1 for negative arguments. And 0 for zero-valued
arguments. Note that negative numbers need to be within brackets when typing into GHCi. -}
sign :: Int -> Int
sign x
  | x == 0 = x
  | x > 0 = 1
  | otherwise = -1

-- 3. Write a function which determines how many of its three arguments are equal (i.e can only return 0, 2, or 3).
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z = 3
  | x /= y && x /= z && y /= z = 0
  | otherwise = 2

{- 4. Write a function which takes the side-lengths of three squares as its arguments, and returns the sum of the
lengths of the squares' diagonals. -}
sumDiagonalLength :: Float -> Float -> Float -> Float
sumDiagonalLength x y z = diag x + diag y + diag z
  where
    diag a = sqrt (2 * a ^ 2)

{- 5. A taxi company calculates fares based on distance travelled. Fares start at £2.20; 50p is added for each kilometre
covered for the first 10 kilometres; and 30p is added for each additional kilometre. Write a function which takes the
distance in kilometres, and returns the fare in pounds. -}
taxiFare :: Int -> Float
taxiFare x
  | x < 10 = fromIntegral x * 0.5 + 2.2
  | otherwise = fromIntegral (x - 10) * 0.3 + 10 * 0.5 + 2.2

{- 6. Write a function which returns how many of its three integer arguments are greater than their average value. Hint:
first consider what the possible results could be. -}
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

howManyGreaterThanAvg :: Int -> Int -> Int -> Int
howManyGreaterThanAvg x y z
  | fromIntegral x > avg && fromIntegral y > avg = 2
  | fromIntegral x > avg && fromIntegral z > avg = 2
  | fromIntegral y > avg && fromIntegral z > avg = 2
  | x == y && x == z = 0
  | otherwise = 1
  where
    avg = averageThree x y z

{- 7. Write a function which takes integers representing a day and month, and returns True if, and only if, the date is
valid. For example:
> validDate 29 3

gives True (since 29th March is a valid date), but:
> validDate 30 2
> validDate 25 13
> validDate (-4) 6

all give False. Assume February always has 28 days. -}
validDate :: Int -> Int -> Bool
validDate d m
  | d > 0 && d <= 30 && monthShort = True
  | d > 0 && d <= 28 && m == 2 = True
  | d > 0 && d <= 31 && not (monthShort || m == 2) = True
  | otherwise = False
  where
    monthShort = m == 4 || m == 6 || m == 9 || m == 11

{- 8. Assuming that all years divisible by 4 are leap years, write a function which returns the number of days in a
given month and year. For example:
daysInMonth 2 2012 -> 29
-}
daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m == 2 && mod y 4 == 0 = 29
  | m == 2 = 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31