-- pract06.hs

import Data.Char (isDigit, isLower)

{- Use the higher-order functions map, filter, and foldr from the Prelude to write the following functions. You should
aim for your solutions to be as concise as possible. In particular:
• where possible, give function-level definitions (see page 6 from the lecture)
• don’t declare the types of the functions (let Haskell determine the most general type)

Try to work out what the type of each of your solutions is, and use the :type command to check your understanding -}

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Integer] -> [Integer]
doubleAll = map (* 2)

areDigits :: [Char] -> [Bool]
areDigits = map isDigit

keepPositive :: [Integer] -> [Integer]
keepPositive = filter (> 0)

keepDigits :: [Char] -> [Char]
keepDigits = filter isDigit

addUp :: Num a => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

{- 1) Write a function mult10 that multiplies each element of a list by 10. For example:
> mult10 [5,7,2,4] -> [50,70,20,40]
-}
mult10 :: [Integer] -> [Integer]
mult10 = map (* 10)

{- 2) Write a function onlyLowerCase which removes any character from a string that is not a lower-case letter. For
example:
> onlyLowerCase "Port 15" -> "ort"
-}
onlyLowerCase :: [Char] -> [Char]
onlyLowerCase = filter isLower

{- 3) Write a function orAll which finds the disjunction (or) of the elements in a Boolean list. For example:
> orAll [True,False,True] -> True
> orAll [False,False] -> False
> orAll [] -> False
-}
orAll :: [Bool] -> Bool
orAll = foldr (||) False

{- 4) Write a function sumSquares that returns the sum of the squares of the elements of a list. For example:
> sumSquares [3,2,4] -> 29
-}
sumSquares :: Num c => [c] -> c
sumSquares = sum . map (^ 2)

{- 5) Write a function zeroToTen that keeps only those values that are between 0 and 10 in a list. For example:
> zeroToTen [7,-3,0,15,10,2] -> [7,0,10,2]
-}
zeroToTen :: [Integer] -> [Integer]
zeroToTen = filter (>= 0) . filter (<= 10)

{- 6) Write a function squareRoots that finds the square roots of all the non-negative values in a list. For example:
> squareRoots [4,-8,10] -> [2.0, 3.16]
-}
squareRoots :: [Double] -> [Double]
squareRoots = map sqrt . filter (>= 0)

{- 7) Write a function countBetween that counts the number of items in a list that are between specified lower and upper
bounds. For example:
> countBetween 3 6 [5, 9, 2, 4, 6, 3, 1, 4] -> 5

since 5, 4, 6, 3, & 4 are between the bounds 3 and 6. -}
countBetween :: Ord a => a -> a -> [a] -> Int
countBetween x y = length . filter (>= x) . filter (<= y)

{- 8) Write a function alwaysPositive that tests whether applying a given function to all the elements of a list results
only in positive values. For example:
> alwaysPositive (+10) [-1,-8,2] -> True
> alwaysPositive (*2) [-1,-8,2] -> False) -}
alwaysPositive :: (Ord a1, Num a1) => (a2 -> a1) -> [a2] -> Bool
alwaysPositive f = and . map ((>= 0) . f)

{- 9) Write a function productSquareRoots that finds the product of the square roots of all the non-negative values in a
list. For example:
> productSquareRoots [4,-8,10] -> 6.32
-}
productSquareRoots :: [Double] -> Double
productSquareRoots = product . map sqrt . filter (> 0)

{- 10) Write a function removeFirst that removes the first element of a list that has a given property. For example:
> removeFirst (<0) [3,-1,4,-8,2] -> [3,4,-8,2])
-}
removeFirst :: (Int -> Bool) -> [Int] -> [Int]
removeFirst f [] = []
removeFirst f (x : xs)
  | f x = xs
  | otherwise = x : removeFirst f xs

{- 11) Write a function removeLast that removes the last element of a list that has a given property. For example:
> removeLast (<0) [3,-1,4,-8,2] -> [3,-1,4,2]
-}
removeLast :: (Int -> Bool) -> [Int] -> [Int]
removeLast f = reverse . removeFirst f . reverse

{- Lambda expressions are nameless (i.e. anonymous) functions (the term lambda expression comes from the lambda
calculus, which underlies all functional languages). Lambda expressions are often used as arguments to higher-order
functions such as map, filter, and foldr. To define a lambda expression, we use a backslash (intended to look like the
letter lambda - λ), function arguments, an arrow and a return expression.

We can apply a lambda expression as follows:
> (\x -> 2 * x) 4 -> 8
-}

{- 12) Using filter and a single lambda expression, give an alternative solution to exercise 5. -}
zeroToTenLam :: [Integer] -> [Integer]
zeroToTenLam = filter (\x -> x > 0 && x < 10)

{- 13) Using only lambda expressions and foldr (i.e. not map or filter), write new versions of:
(i) the mult10 function from exercise 1
(ii) reverse (to reverse a list)
(iii) onlyLowerCase from exercise 2
-}
mult10Lam :: [Int] -> [Int]
mult10Lam = foldr (\x xs -> (* 10) x : xs) []

reverseLam :: [a] -> [a]
reverseLam = foldr (\x xs -> xs ++ [x]) []

onlyLowerCaseLam :: [Char] -> [Char]
onlyLowerCaseLam = foldr (\x xs -> if isLower x then x : xs else xs) []