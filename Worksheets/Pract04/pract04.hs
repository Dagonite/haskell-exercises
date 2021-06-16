-- pract04.hs

import Data.Char (isDigit, toUpper)

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stMarks = [mk | (st, mk) <- stMarks]

pass :: [StudentMark] -> [String]
pass stMarks = [st | (st, mk) <- stMarks, mk >= 40]

-- An example list of student marks
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

{- The pass function will return all students from the list who have a passing score. In other words:
> pass testData -> ["John", "Kate", "Jill", "Jack", "Sue"]
-}

{- 1) Write a function which returns both the sum and the difference between the first and second arguments. For
example:
> sumDifference 3 7 -> (10,-4)
-}
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

{- 2) Write a function which returns a student’s grade from a percentage mark. Marks of 70 or above get an A grade,
marks between 60 and 69 get a B, between 50 and 59 a C, between 40 and 49 a D, and marks below 40 get an F. For exmaple:
> grade ("James", 78) -> 'A'
-}
grade :: StudentMark -> Char
grade (x, y)
  | y >= 70 = 'A'
  | y >= 60 = 'B'
  | y >= 50 = 'C'
  | y >= 40 = 'D'
  | otherwise = 'F'

{- 3) Write a function which caps the mark of a student to a maximum of 40. For example:
> capMark ("James", 78) -> ("James, 40")
-}
capMark :: StudentMark -> StudentMark
capMark (x, y)
  | y > 40 = (x, 40)
  | otherwise = (x, y)

{- 4) Write a function that gives a list of the first n positive integers. For example:
> firstNumbers 3 -> [1, 2, 3]
-}
firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

{- 5) Write a function that gives a list of the first n squares. For exmaple:
> firstSquares 3 -> [1, 4, 9]
-}
firstSquares :: Int -> [Int]
firstSquares x = [i ^ 2 | i <- firstNumbers x]

{- 6) Write a function which converts all the small letters in a string to capitals. For example:
> capitalise "Po1 3he" -> "PO1 3HE"
-}
capitalise :: String -> String
capitalise x = [toUpper i | i <- x]

{- 7) Write a function that strips all non-digit characters from a string. For example:
> onlyDigits "ac245d62" -> "24562"
-}
onlyDigits :: String -> String
onlyDigits x = [i | i <- x, isDigit i]

{- 8) Using your capMark function and a list comprehension, write a function which caps all students’ marks to a maximum
of 40%. For example:
> capMarks [("Jo",37),("Sam",76)] -> [("Jo", 37), ("Sam", 40)]
-}
capMarks :: [StudentMark] -> [StudentMark]
capMarks m = [capMark i | i <- m]

{- 9) Using your grade function and a list comprehension, write a function which grades a list of students marks. For
example:
> gradeStudents [("Jo",47),("Sam",76)] -> [("Jo",’D’), ("Sam",’A’)]
-}
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents g = [(fst i, grade i) | i <- g]

{- 10) Using recursion, write a function which joins copies of a string together. For example:
> duplicate "Hi" 3 -> "HiHiHi"
-}
duplicate :: String -> Int -> String
duplicate x y
  | y > 0 = x ++ duplicate x (y - 1)
  | otherwise = ""

{- 11) Using list comprehension, write a function which returns the list of the divisors of a positive integer (and []
for any other integer. For example:
> divisors 15 -> [1, 3, 5, 15]
-}
divisors :: Int -> [Int]
divisors x = [i | i <- [1 .. x], x `mod` i == 0]

{- 12) Using your divisors function, write a function which tests whether an integer is a prime number. Hint: how many
divisors do prime numbers have? -}
isPrime :: Int -> Bool
isPrime x
  | length (divisors x) == 2 = True
  | otherwise = False

{- 13) Using list comprehensions, write a polymorphic function which transforms a list of pairs (of any types) into a
pair of lists. For example:
> split [(1,'a'),(2,'b'),(3,'c')] -> ([1,2,3], "abc")
-}
split :: [(a, b)] -> ([a], [b])
split x = ([fst i | i <- x], [snd i | i <- x])