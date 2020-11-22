{- The first line is a declaration of the function's type. It states that the
function mult2 takes an Int value and returns another Int. The second line is
the definition of the function. It states that for any argument value x, the
function returns the value 2 * x -}
mult2 :: Int -> Int
mult2 x = 2 * x

-- can see what this does by typing `mult2 4` into the GHCi prompt
-- can also try `mult2 (3+1)` and `mult2 3 + 1`

mult4 :: Int -> Int
mult4 x = mult2 (mult2 x)

{- All GHCi commands begin with a colon and are not part of the Haskell
language, they are used within the GHCi environment to change settings, to
navigate around the filesystem and to edit, load, and reload Haskell files -}

-- try `:type mult2 3`

-- Note that, in functional programming, functions themselves are expressions
-- (and therefore have types). Try, for example: `:type mult2`

{- Can use non-GUI GHCi for file nagivation as it includes code completion:
:cd Users/Dagonite/Documents/GitHub/haskell-projects/Worksheets/Pract01
:!cd -- shows the current directory path
:!dir -- lists files in the current directory
:load pract01 -- loads the pract01.hs file
:reload -- reloads file if changes have been made since last loaded
:edit -- load the editor from within GHCi
:quit -- exit GHCi

The common GHCi commands can be abbreviated as follows:
:type >>> :t
:load >>> :l
:reload >>> :r
:edit >>> :e
:quit >>> :q
-}

-- Function to multiply an input by pi
multPi :: Float -> Float
multPi x = pi * x

{- 1. Write a function which multiplies its argument by 10 (e.g timesTen 5 gives
50) -}
timesTen :: Int -> Int
timesTen x = 10 * x

-- 2. Write a function which gives the sum of three integers
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

{- 3. Using the constant pi and the power operator ^, write a function which
gives the area of a circle given its radius -}
piApprox :: Float
piApprox = 22/7 -- Can be used instead of pi

areaOfCircle :: Float -> Float
areaOfCircle x = pi * x^2

{- 4. Using the definition of areaOfCircle, write a function that gives the
volume of a cylinder given its length and cross-sectional radius -}
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x y = areaOfCircle x * y

{- 5. Write a function that takes four floats representing the coordinates x1,
y1, x2, y2 of two points, and gives the distance between the points -}
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1 - y2)^2 + (x1 - x2)^2)

{- 6. Write a function which returns True if, and only if, all of its three
arguments are all different from one another -}
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

{- 7. Using the mod function, write a function that tests whether one integer is
divisible by another -}
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

{- 8. Using the definition of divisibleBy, write a function which determines
whether its argument is an even number -}
isEven :: Int -> Bool
isEven x = divisibleBy x 2

{- 9.  Write a function:

averageThree :: Int -> Int -> Int -> Float

which gives the average of three integer values. Note that Haskell will treat
the inputs as Ints, and the output will need to be a Float (since those are the
types given in the type declaration), but it doesn’t automatically convert from
one type to the other. Use the fromIntegral function to do this -}
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (sumThree x y z) / 3

{- 10. Using a conditional expression (and not the built-in function abs), write
a function that gives the absolute value of an integer (i.e. gives a
non-negative value):

absolute :: Int -> Int

Note that if you write:

f -3

where f is any function in Haskell, it will be interpreted as (f -) 3 and thus
give an error – you’ll see why in a later lecture. You may need to use your own
parentheses in order to avoid such an error  -}
absolute :: Int -> Int
absolute x = if x > 0
                then x
                else x * (-1)
