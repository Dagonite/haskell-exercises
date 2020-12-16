-- fizzbuzz.hs

fizzBuzz :: Int -> String
fizzBuzz n
  | fizz && buzz = "FizzBuzz"
  | buzz = "Buzz"
  | fizz = "Fizz"
  | otherwise = show n
  where
    fizz = mod n 3 == 0
    buzz = mod n 5 == 0

fizzBuzzRange :: Int -> IO ()
fizzBuzzRange x = mapM_ (print . fizzBuzz) [1 .. x]
