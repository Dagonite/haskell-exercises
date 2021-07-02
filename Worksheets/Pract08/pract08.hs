-- pract08.hs

helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
  putStr "Enter the filename: "
  name <- getLine
  contents <- readFile name
  putStr contents

getInt :: IO Int
getInt = do
  str <- getLine
  return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
  | str == reverse str = str ++ " is a palindrome"
  | otherwise = str ++ " is not a palindrome"

pal :: IO ()
pal = do
  line <- getLine
  let response = isPalindrome line
  putStrLn response

palLines :: IO ()
palLines = do
  putStr "Enter a line: "
  str <- getLine
  if str == ""
    then return ()
    else do
      putStrLn (isPalindrome str)
      palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9

celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

{- 1) Write a greeting program that asks the user for his or her name, and then outputs a personalised greeting. For
example, on input "Sam", the program should display "Hello, Sam". -}
greeting :: IO String
greeting = do
  putStr "Enter your name > "
  name <- getLine
  return ("Hello, " ++ name)

-- 2) Write an addTwoNumbers program that reads two integers, each on a separate line, and displays their sum.
addTwoNumbers :: IO [Char]
addTwoNumbers = do
  putStr "Enter a number > "
  x <- getLine
  putStr "Enter another number > "
  y <- getLine
  let total = (read x :: Int) + (read y :: Int)
  return ("Sum: " ++ show total)

{- 3) Write a copyFile program that copies a text-file. The program should ask the user for the name of the file to copy
and the name that the copy should be called. -}
copyFile :: IO ()
copyFile = do
  putStr "Enter a file name (try 'test.txt') > "
  name <- getLine
  contents <- readFile name
  putStr "Enter a name for the file copy > "
  copy <- getLine
  writeFile copy contents

{- 4) Write a program that allows the user to build up a list of strings by entering them in one at a time, and displays
the list after every step. The program should exit when the user enters an empty string. An example execution is as
follows:
> Enter a line: hello
> List is now ["hello"]
> Enter a line: world
> List is now ["hello","world"]
> Enter a line:

Hint: use two definitions - a recursive
buildList :: [String] -> IO ()

function that builds up a list step-by-step, and another function listBuilder of type IO () that starts off the
computation by calling buildList with a suitable initial parameter value. -}
buildList :: [String] -> IO ()
buildList arr = do
  putStr "Enter a line: "
  line <- getLine
  if line == ""
    then return ()
    else do
      let newArr = arr ++ [line]
      putStrLn ("List is now " ++ show newArr)
      buildList newArr

listBuilder :: IO ()
listBuilder = do
  buildList []

{- 5) Write a program that reads an integer n from the user, then reads n integers (on separate lines), and finally
displays the sum of the n numbers read. Again, use two functions. -}
addUp :: Num p => [p] -> p
addUp [] = 0
addUp (x : xs) = sum (x : xs)

buildAddUpList :: [Int] -> IO ()
buildAddUpList arr = do
  putStr "Enter a number: "
  n <- getLine
  if n == ""
    then putStrLn ("The sum of the numbers is: " ++ show (addUp arr))
    else do
      let newArr = arr ++ [read n :: Int]
      putStrLn ("List is now " ++ show newArr)
      buildAddUpList newArr

addUpBuilder :: IO ()
addUpBuilder = do
  buildAddUpList []

{- 6) [harder] The Week9.hs file contains two pure functions for converting between Fahrenheit and Celsius temperatures.
Using these functions, write a program that allows the user to enter any number of temperatures, and then displays the
average of all the temperatures entered. After entering each value, the user should be prompted to enter either an “f”
or a “c” to say whether the entered value was in Fahrenheit or Celsius. The user should type a special value (e.g. the
empty string or “stop”) to finish. The average temperature should be outputted in both Fahrenheit and Celsius. -}