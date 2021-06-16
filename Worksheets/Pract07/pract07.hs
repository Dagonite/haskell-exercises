-- pract07.hs

-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

isWeekend2 :: Day -> Bool
isWeekend2 day = day == Sat || day == Sun

isWeekend3 :: Day -> Bool
isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
  deriving (Eq, Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
  | m1 >= m2 = s1
  | otherwise = s2

-- Shapes algebraic type
data Shape
  = Circle Float
  | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
  deriving (Show)

data Building
  = Name String
  | Number Int
  deriving (Show)

-- Binary tree algebraic type
data Tree
  = Null
  | Node Int Tree Tree
  deriving (Show)

-- Binary tree test data
testTree :: Tree
testTree =
  Node
    20
    (Node 3 (Node 12 Null Null) (Node 7 Null Null))
    (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

--        20
--      /    \
--     3      8
--    / \    /
--   12  7  4
--         /
--        6

-- Binary search tree test data
testSearchTree :: Tree
testSearchTree =
  Node
    5
    (Node 1 Null Null)
    (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

{- 1) Define two algebraic types:
• Month to represent the twelve months of the year
• Season to represent the four seasons
-}
data Month = December | January | February | March | April | May | June | July | August | September | October | November
  deriving (Eq, Ord, Show, Read)

data Season = Spring | Summer | Autumn | Winter
  deriving (Eq, Ord, Show, Read)

{- 2) Define a function:
season :: Month -> Season

which maps months onto seasons (assume season February = Winter, season March = Spring, and that seasons are all three
months long). Try to make your definition as short as possible. -}
season :: Month -> Season
season m
  | m < March = Winter
  | m < June = Spring
  | m >= September = Summer
  | otherwise = Autumn

{- 3) Define a function:
numberOfDays :: Month -> Int -> Int

which gives the number of days a month has in a given year. Assume all years divisible by four are leap years. For
example:
> numberOfDays February 2012 -> 29
-}
numberOfDays :: Month -> Int -> Int
numberOfDays m y
  | m == February && y `mod` 4 == 0 = 29
  | m == February = 28
  | m == April || m == June || m == September || m == November = 30
  | otherwise = 31

-- 4) Define an algebraic type Point for representing (the coordinates of) points in two-dimensional space.
data Point = Point Float Float
  deriving (Show)

{- 5) Using Point, define a modified version PositionedShape of the Shape data type which includes the centre point of a
shape, in addition to its dimensions. -}
data PositionedShape
  = PositionedCircle Point Float
  | PositionedRectangle Point Float Float
  deriving (Show)

{- 6) Define a function:
move :: PositionedShape -> Float -> Float -> PositionedShape

which moves a shape by the given x and y distances. -}
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedCircle (Point x1 y1) r) x2 y2 = PositionedCircle (Point (x1 + x2) (y1 + y2)) r
move (PositionedRectangle (Point x1 y1) h w) x2 y2 = PositionedRectangle (Point (x1 + x2) (y1 + y2)) h w

{- 7) Define, for the binary tree type Tree, a function:
numberOfNodes :: Tree -> Int

which returns the number of nodes there are in a given binary tree. -}
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ st1 st2) = 1 + numberOfNodes st1 + numberOfNodes st2

{- 8) Define a function:
isMember :: Int -> Tree -> Bool

which tests whether a given value is in a tree. -}
isMember :: Int -> Tree -> Bool
isMember x Null = False
isMember x (Node x' st1 st2) = (x == x') || isMember x st1 || isMember x st2

{- 9) Define a function:
leaves :: Tree -> [Int]

which gives a list of the leaves of the tree (i.e. those nodes with null left and right subtrees). -}
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node x Null Null) = [x]
leaves (Node _ st1 st2) = leaves st1 ++ leaves st2

{- 10) Define a function:
inOrder :: Tree -> [Int]

which lists the elements of a tree according to an in-order traversal. (If the tree is a valid binary search tree, then
this function will give a list of the tree's elements in ascending numerical order). -}
inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node x Null Null) = [x]
inOrder (Node x st1 st2) = inOrder st1 ++ [x] ++ inOrder st2

{- 11) Define a function:
insert :: Int -> Tree -> Tree

which inserts a new value into a tree. The function should assume that the tree is a binary search tree and should
preserve this property. -}
insert :: Int -> Tree -> Tree
insert x Null = Node x Null Null
insert x (Node x' st1 st2)
  | x == x' = Node x' st1 st2
  | x < x' = Node x' (insert x st1) st2
  | otherwise = Node x' st1 (insert x st2)

-- Example:
-- insert 12 Null -> Node 12 Null Null
-- insert 10 it   -> Node 12 (Node 10 Null Null) Null
-- insert 19 it   -> Node 12 (Node 10 Null Null) (Node 19 Null Null)
-- insert 1 it    -> Node 12 (Node 10 (Node 1 Null Null) Null) (Node 19 Null Null)
-- insert 3 it    -> Node 12 (Node 10 (Node 1 Null (Node 3 Null Null)) Null) (Node 19 Null Null)
-- insert 2 it    -> Node 12 (Node 10 (Node 1 Null (Node 3 (Node 2 Null Null) Null)) Null) (Node 19 Null Null)
-- insert 11 it   -> Node 12 (Node 10 (Node 1 Null (Node 3 (Node 2 Null Null) Null)) (Node 11 Null Null)) (Node 19 Null Null)

--         12
--       /    \
--      10    19
--     /  \
--    1   11
--     \
--      3
--     /
--    2

-- In-order traversal:
-- 1 2 3 10 11 12 19

{- 12) [harder] Define a function:
listToSearchTree :: [Int] -> Tree

which creates a binary search tree by inserting into an initially empty tree the elements
from a list in the order in which they appear. For example:
> listToSearchTree [2,1,3] -> Node 2 (Node 1 Null Null) (Node 3 Null Null)

Finally, using listToSearchTree and inOrder, write another function:
binaryTreeSort :: [Int] -> [Int]

that sorts a list of integers. -}
listToSearchTree :: [Int] -> Tree
listToSearchTree = foldr insert Null

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort x = inOrder (listToSearchTree x)