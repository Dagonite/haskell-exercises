-- pract03.hs
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- We don't import these from the Prelude so that we can define our own versions
import Prelude hiding (gcd, (&&), (||))

{- The following line declares the || operator (which we are about to re-define) to be right associative and to have
precedence 2. This is necessary in order for expressions such as False || x > 2 to be valid (e.g. it sets the precedence
of || to be lower than >). -}
infixr 2 ||

{-
A naive re-implementation of the Prelude operator ||:
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

An alternative re-implementation:
(||) :: Bool -> Bool -> Bool
False || False   = False
_ || _           = True
-}

-- I will go with this re-implementation:
(||) :: Bool -> Bool -> Bool
True || _ = True
False || a = a

-- Recursion and pattern guards to make functions for factorials, multiplying, and dividing -}
fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

{- 1) Following the examples of the re-implementations of the || (or) operator, write three definitions of the && (and)
operator using pattern matching. Remember to first hide the definition given in the standard Prelude, and to include the
fixity declaration: infixr 3 && to force complex Boolean expressions to be correctly interpreted. Make sure your three
definitions mirror those of the definitions of || given in the file and described in the lecture notes:
(i) on slide 6
(ii) at the top of slide 7
(iii) at the bottom of slide 7

after writing and testing each operator, comment out the definition using "--" to allow you to give the next
definition. -}
infixr 3 &&

{-
A naive re-implementation of the Prelude operator &&:
(&&) :: Bool -> Bool -> Bool
True && True    = True
False && True   = False
True && False   = False
False && False  = False

An alternative re-implementation
(&&) :: Bool -> Bool -> Bool
True && True   = True
_ && _         = False
-}

-- I will go with this re-implementation:
(&&) :: Bool -> Bool -> Bool
True && a = a
False && _ = False

{- 2) Using pattern matching, define an exclusive or function which gives True when exactly one of its arguments is
True. -}
exOr :: Bool -> Bool -> Bool
True `exOr` True = False
False `exOr` True = True
True `exOr` False = True
False `exOr` False = False

{- 3) Define a function which gives its second argument if the condition (the first argument) is True, and the third
argument if the condition is False. For example:
> ifThenElse (3 > 5) 7 12 -> 12
-}
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse x y z
  | x = y
  | otherwise = z

{- 4) Use pattern matching to write a function which takes an integer (assumed to be between 1 and 12) and returns the
number of days in the corresponding month. For example:
> daysInMonth 1 -> 31
> daysInMonth 2 -> 28

try to make your solution as short as possible. Using the daysInMonth function, write a new (simpler) version of your
validDate function from the previous worksheet (no guards should be necessary). -}
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

--daysInMonth :: Int -> Int
--daysInMonth m
--    | m == 2 = 28
--    | m == 4 || m == 6 || m == 9 || m == 11 = 30
--    | otherwise = 31

validDate :: Int -> Int -> Bool
validDate d m = daysInMonth m >= d

{- 5) Write a recursive function that gives the sum of the first n positive integers. For example:
> sumNumbers 3 -> 6
-}
sumNumbers :: Int -> Int
sumNumbers x
  | x > 0 = x + sumNumbers (x - 1)
  | otherwise = x

{- 6) Write a recursive function that gives the sum of the first n squares. For example:
> sumSquares 3 -> 14
-}
sumSquares :: Int -> Int
sumSquares x
  | x > 0 = x ^ 2 + sumSquares (x - 1)
  | otherwise = x

{- 7) Using multiplication, write a recursive function which raises its first argument to the power of the second. For
example:
> power 2 3 -> 8)
-}
power :: Int -> Int -> Int
power x y
  | y > 1 = x * power x (y - 1)
  | y == 1 = x
  | otherwise = error "powers not defined for negative ints"

{- 8) Write a recursive function that gives the sum of all integers between and including its two arguments. For
example:
> sumFromTo 5 8 -> 26

if the first argument is greater than the second, your function should return 0. -}
sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | y > x = x + sumFromTo (x + 1) y
  | otherwise = 0

{- 9) One definition of the greatest common divisor (GCD) of two non-negative integers is:
• the GCD of two equal integers is their common value
• the GCD of two non-equal integers is the GCD of their positive difference and the smaller integer

write a function for finding the GCD of two non-negative integers, based on the above definition. Note: you'll need to
hide the definition of gcd given in the Prelude. -}
gcd :: Int -> Int -> Int
gcd x y
  | x > y = gcd (x - y) y
  | y > x = gcd (y - x) x
  | otherwise = x

{- 10) The integer square root of a positive integer n is the largest integer whose square is less than or equal to n
(e.g. the integer square root of 7 is 2, and that of 9 is 3). Use recursion to write a function that computes the
integer square root of a number. Hint: intSquareRoot should be non-recursive, but it should call a recursive function
which has two parameters. -}
intSquareRoot :: Int -> Int
intSquareRoot n = aux n
  where
    aux x
      | x ^ 2 > n = aux (x - 1)
      | otherwise = x

{- 11) If many of your answers to questions 5 to 10 involve guards, write versions of three of them that use pattern
matching and no guards. -}
ifThenElsePM :: Bool -> Int -> Int -> Int
ifThenElsePM True x y = x
ifThenElsePM _ x y = y

powerPM :: Int -> Int -> Int
powerPM x 0 = 1
powerPM x 1 = x
powerPM x y =
  if y > 1
    then x * powerPM x (y - 1)
    else error "powers not defined for negative ints"

sumSquaresPM :: Int -> Int
sumSquaresPM 0 = 0
sumSquaresPM 1 = 1
sumSquaresPM x =
  if x > 1
    then x ^ 2 + sumSquaresPM (x - 1)
    else x