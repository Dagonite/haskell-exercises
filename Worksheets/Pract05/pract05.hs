-- pract05.hs
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

-- Definitions of the Prelude functions fst and snd
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

-- Definitions of the Prelude functions head and tail
head :: [a] -> a
head (x : _) = x

tail :: [a] -> [a]
tail (_ : xs) = xs

type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71),
    ("John", 71),
    ("Sam", 39),
    ("Kate", 75),
    ("Jill", 45),
    ("Bill", 41),
    ("Amy", 60),
    ("Jack", 12),
    ("Sue", 89)
  ]

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum = foldr (+) 0

doubleAll :: [Int] -> [Int]
doubleAll = map (2 *)

concat :: [[a]] -> [a]
concat = foldr (++) []

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- 1) Write a function which returns the first element plus one for a non-empty list, and 0 for an empty list.
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x : xs) = x + 1

-- 2) Write a polymorphic function which adds an extra copy of the first element at the beginning of the list.
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : x : xs

-- 3) Write a polymorphic function which swaps the first two elements of a list
rotate :: [a] -> [a]
rotate [] = []
rotate (x1 : x2 : xs) = x2 : x1 : xs

{- 4) Write a recursive polymorphic function which returns the length of a list (i.e. re-implementation of the Prelude's
length function). -}
listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

-- 5) Write a recursive function which returns the product of all the integers in the list.
multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

-- 6) Write a recursive function which returns the conjunction (and) of all the elements of a list.
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = x && andAll xs

{- 7) which counts the number of times a given value appears in a list. Hint: this function may require two recursive
cases. -}
countElems :: Int -> [Int] -> Int
countElems x [] = 0
countElems x (y : ys)
  | x == y = 1 + countElems x ys
  | x /= y = countElems x ys

-- 8) Write a recursive function that removes all copies of a given value from a list of integers.
removeAll :: Int -> [Int] -> [Int]
removeAll x [] = []
removeAll x (y : ys)
  | x == y = removeAll x ys
  | otherwise = y : removeAll x ys

{- 9) Recall the StudentMark type synonym from last week. Write a recursive function which gives a list of the marks for
a particular student. -}
listMarks :: String -> [StudentMark] -> [Int]
listMarks x [] = []
listMarks x (y : ys)
  | x == fst y = snd y : listMarks x ys
  | otherwise = listMarks x ys

{- 10) which decides if the first list is a prefix of the second list (e.g. [1,4] is a prefix of [1,4,9,2], and [] is a
prefix of any list). -}
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x : xs) (y : ys)
  | x == y = prefix xs ys
  | otherwise = False

-- 11) Using the prefix function, write a recursive function which decides if the first list is contained in the second.
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence x (y : ys)
  | prefix x (y : ys) = True
  | otherwise = subSequence x ys

-- 12) Rewrite questions 4, 5, and 6 without using recursion.
listLength2 :: [a] -> Int
listLength2 = foldr (\x -> (+) 1) 0

multAll2 :: [Int] -> Int
multAll2 = product

andAll2 :: [Bool] -> Bool
andAll2 = and