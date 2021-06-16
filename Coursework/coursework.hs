-- coursework.hs

-- To start the first demo:
-- Prelude> demo 1

-- To start the user interface:
-- Prelude> main

-- =======
-- Imports
-- =======

import Data.List (findIndex, isPrefixOf, nub)
import Data.Maybe (fromMaybe, isNothing)
import Text.Printf (printf)

-- =====
-- Types
-- =====

data Album = Album
  { title :: String,
    artist :: String,
    year :: Int,
    sales :: Int
  }
  deriving (Show, Read)

testData :: [Album]
testData =
  [ Album "Greatest Hits" "Queen" 1981 6300000,
    Album "Gold: Greatest Hits" "ABBA" 1992 5400000,
    Album "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles" 1967 5340000,
    Album "21" "Adele" 2011 5110000,
    Album "(What's the Story) Morning Glory?" "Oasis" 1995 4940000,
    Album "Thriller" "Michael Jackson" 1982 4470000,
    Album "The Dark Side of the Moon" "Pink Floyd" 1973 4470000,
    Album "Brothers in Arms" "Dire Straits" 1985 4350000,
    Album "Bad" "Michael Jackson" 1987 4140000,
    Album "Rumours" "Fleetwood Mac" 1977 4090000,
    Album "Greatest Hits II" "Queen" 1991 3990000,
    Album "Back to Black" "Amy Winehouse" 2006 3940000,
    Album "The Immaculate Collection" "Madonna" 1990 3700000,
    Album "25" "Adele" 2015 3500000,
    Album "Stars" "Simply Red" 1991 3450000,
    Album "Come On Over" "Shania Twain" 1998 3430000,
    Album "x" "Ed Sheeran" 2014 3380000,
    Album "Legend" "Bob Marley" 1984 3380000,
    Album "Bat Out of Hell" "Meat Loaf" 1977 3370000,
    Album "Back to Bedlam" "James Blunt" 2004 3360000,
    Album "Urban Hymns" "The Verve" 1997 3340000,
    Album "Bridge over Troubled Water" "Simon & Garfunkel" 1970 3260000,
    Album "1" "The Beatles" 2000 3230000,
    Album "Spirit" "Leona Lewis" 2007 3170000,
    Album "Crazy Love" "Michael Bublé" 2009 3130000,
    Album "No Angel" "Dido" 2000 3090000,
    Album "White Ladder" "David Gray" 1998 3020000,
    Album "The Fame" "Lady Gaga" 2009 2990000,
    Album "Only by the Night" "Kings of Leon" 2008 2980000,
    Album "A Rush of Blood to the Head" "Coldplay" 2002 2960000,
    Album "Talk on Corners" "The Corrs" 1997 2960000,
    Album "Spice" "Spice Girls" 1996 2960000,
    Album "Life for Rent" "Dido" 2003 2900000,
    Album "Beautiful World" "Take That" 2006 2880000,
    Album "The Joshua Tree" "U2" 1987 2880000,
    Album "Hopes and Fears" "Keane" 2004 2860000,
    Album "The War of the Worlds" "Jeff Wayne" 1978 2800000,
    Album "X&Y" "Coldplay" 2005 2790000,
    Album "Jagged Little Pill" "Alanis Morissette" 1995 2780000,
    Album "Tubular Bells" "Mike Oldfield" 1973 2760000,
    Album "Scissor Sisters" "Scissor Sisters" 2004 2760000,
    Album "...But Seriously" "Phil Collins" 1989 2750000,
    Album "Tracy Chapman" "Tracy Chapman" 1988 2710000,
    Album "Parachutes" "Coldplay" 2000 2710000,
    Album "The Man Who" "Travis" 1999 2687500,
    Album "Greatest Hits" "ABBA" 1975 2606000,
    Album "I've Been Expecting You" "Robbie Williams" 1998 2586500,
    Album "Come Away with Me" "Norah Jones" 2002 2556650,
    Album "Graceland" "Paul Simon" 1986 2500000,
    Album "Ladies & Gentlemen: The Best of" "George Michael" 1998 2500000
  ]

-- =========
-- Functions
-- =========

-- Each list element converted into a string which is then formatted and printed into four separate columns
formatAlbum :: Album -> String
formatAlbum Album {title = ti, artist = ar, year = yr, sales = sl} =
  printf "%-40s%-20s%-8d%d" ti ar yr sl

albumsToString :: [Album] -> String
albumsToString = foldr ((\a b -> a ++ "\n" ++ b) . formatAlbum) []

-- Takes the first 10 elements (albums already in sales order)
top10 :: [a] -> [a]
top10 = take 10

-- Bool function which returns true if the year of the album is between the two given years (inclusive)
betweenInclusive :: Ord a => a -> a -> a -> Bool
betweenInclusive low high yr = (yr >= low) && (yr <= high)

-- Albums which return false are filtered out so aren't converted to strings and printed for demo 3
albumsBetweenYears :: Int -> Int -> [Album] -> [Album]
albumsBetweenYears low high = filter (betweenInclusive low high . year)

-- Bool function which returns true if the album title has the given prefix
titlePrefix :: [Char] -> Album -> Bool
titlePrefix pre x = pre `isPrefixOf` title x

-- Albums which return false are filtered out so aren't converted to strings and printed for demo 4
albumsWithPrefix :: [Char] -> [Album] -> [Album]
albumsWithPrefix pre = filter (titlePrefix pre)

-- Function which returns a list of albums with a matching artist to the the one given
albumsByArtist :: String -> [Album] -> [Album]
albumsByArtist ar = filter (\a -> artist a == ar)

{- The returned list of albums with the same artist have their sales mapped into a list. Each value is summed together
to give the total sales -}
totalArtistSales :: String -> [Album] -> String
totalArtistSales ar xs = show (sum (map sales (albumsByArtist ar xs)))

{- Function which returns a list of artists with how many albums they have in the top 50. This number is calculated
using length of albumsByArtist which returns the number of times true is returned for an artist. That artist is mapped
to this number. Duplicates from multiple albums are removed using nub -}
countArtists :: [Album] -> [(String, Int)]
countArtists xs = nub (map (\a -> (artist a, length (albumsByArtist (artist a) xs))) xs)

-- Each artist is formatted and printed next to their number of albums in the top 50
formatArtistCount :: (String, Int) -> String
formatArtistCount (ar, count) = printf "%-20s%d" ar count

-- Same as albumsToString where each list element is converted into a string
artistsToString :: [(String, Int)] -> [Char]
artistsToString = foldr ((\a b -> a ++ "\n" ++ b) . formatArtistCount) []

{- Finds the position in the list of albums where the new entry will be placed by finding the first instance where it
has more sales. Using fromMaybe so at most it is 49th and doesn't return 'Nothing' -}
findPosition :: Int -> [Album] -> Int
findPosition sl ys = fromMaybe 49 (findIndex (\a -> sl >= sales a) ys)

{- Have to do 'init' separately from newEntry so the last element of the albums isn't removed when increasing sales on
an album -}
removeLast :: Album -> [Album] -> [Album]
removeLast x ys = newEntry x (init ys)

newEntry :: Album -> [Album] -> [Album]
newEntry x ys = addAlbum x (findPosition (sales x) ys) ys

{- List of albums checked to see if the given album is already there. If no index is returned then newEntry is called
otherwise the original list is returned -}
searchListForAlbum :: Album -> [Album] -> [Album]
searchListForAlbum x ys
  | isNothing (findAlbum (title x) (artist x) ys) = removeLast x ys
  | otherwise = error "this album is already in the list"

-- List of albums split into two at the index. Lists concatted with the new album in it's own list in the middle
addAlbum :: a -> Int -> [a] -> [a]
addAlbum x i ys =
  let (left, right) = splitAt i ys
   in left ++ [x] ++ right

{- Function which removes an element by splitting the list of albums at the index then concatting them with the tail of
the right list so the head element is removed -}
removeAlbum :: Int -> [a] -> [a]
removeAlbum i xs =
  let (left, right) = splitAt i xs
   in left ++ tail right

{- Uses the given album title and artist to find its index in the list of albums. Nothing happens if the album doesn't
exist in the list (can't use fromMaybe here as need to check if album exists) -}
findAlbum :: String -> String -> [Album] -> Maybe Int
findAlbum ti ar = findIndex (\x -> (title x == ti) && (artist x == ar))

-- Readds the album at the new index
increaseAlbumSales :: String -> String -> Int -> [Album] -> [Album]
increaseAlbumSales ti ar sl xs =
  let i = fromMaybe 49 (findAlbum ti ar xs)
      x = xs !! i
   in newEntry
        Album
          { title = ti,
            artist = ar,
            year = year x,
            sales = sales x + sl
          }
        (removeAlbum i xs)

-- =====
-- Demos
-- =====

demo :: Int -> IO ()
demo 1 = putStrLn (albumsToString testData)
demo 2 = putStrLn (albumsToString (top10 testData))
demo 3 = putStrLn (albumsToString (albumsBetweenYears 2000 2008 testData))
demo 4 = putStrLn (albumsToString (albumsWithPrefix "Th" testData))
demo 5 = putStrLn (totalArtistSales "Queen" testData)
demo 6 = putStrLn (artistsToString (countArtists testData))
demo 7 = putStrLn (albumsToString (searchListForAlbum Album {title = "Progress", artist = "Take That", year = 2010, sales = 2700000} testData))
demo 8 = putStrLn (albumsToString (increaseAlbumSales "21" "Adele" 400000 testData))
demo 9 = putStrLn (albumsToString (searchListForAlbum Album {title = "Greatest Hits", artist = "Queen", year = 1981, sales = 6300000} testData))
demo _ = putStrLn "There is no demo with this number"

-- ==============
-- User interface
-- ==============

main :: IO ()
main = do
  contents <- readFile "albums.txt"
  let db = read contents
  putStrLn (albumsToString db)
  menu db

menu :: [Album] -> IO ()
menu db = do
  putStrLn "\nChoose an option from below:\n"
  putStrLn "1) Return the list of the top 50 albums"
  putStrLn "2) Return the top 10 ablums in descending sales order"
  putStrLn "3) Return the albums realised between two given years (inclusive)"
  putStrLn "4) Return the albums where the title has a given prefix"
  putStrLn "5) Return the sales of a given artist"
  putStrLn "6) Return the number of albums in the top 50 per artist"
  putStrLn "7) Return the albums after removing the 50th and adding a new album"
  putStrLn "8) Return the albums after increasing the sales of an album"
  putStrLn "9) Save the current albums to the text file and exit\n"
  option <- getLine
  case option of
    "1" -> inputShowAlbums db
    "2" -> inputTop10 db
    "3" -> inputAlbumsBetweenYears db
    "4" -> inputTitlePrefix db
    "5" -> inputArtistSales db
    "6" -> inputAlbumsPerArist db
    "7" -> inputNewEntry db
    "8" -> inputIncreaseAlbumSales db
    "9" -> do
      writeToDatabase db
    _ -> do
      putStrLn "\nPlease input a number 1 through 9"
      menu db

-- Option 1
inputShowAlbums :: [Album] -> IO ()
inputShowAlbums db = do
  if null (albumsToString db)
    then do
      putStrLn "\nThere are no albums in the database\n"
    else do
      putStrLn ("\n" ++ albumsToString db)
  menu db

-- Option 2
inputTop10 :: [Album] -> IO ()
inputTop10 db = do
  putStrLn ("\n" ++ albumsToString (take 10 db))
  menu db

-- Option 3
inputAlbumsBetweenYears :: [Album] -> IO ()
inputAlbumsBetweenYears db = do
  putStrLn "\nLowest year (between 1900 and 2020):"
  lowest <- getLine
  putStrLn "\nHighest year (between 1900 and 2020):"
  highest <- getLine
  let l = read lowest :: Int
      h = read highest :: Int
  if ((l > 1899 && l <= 2020) && (h > 1899 && h <= 2020)) && (l <= h)
    then do
      putStrLn (albumsToString (albumsBetweenYears l h db))
    else do
      putStrLn "\nInvalid years given"
  menu db

-- Option 4
inputTitlePrefix :: [Album] -> IO ()
inputTitlePrefix db = do
  putStrLn "\nEnter a prefix:"
  pre <- getLine
  putStrLn ("\n" ++ albumsToString (albumsWithPrefix pre db))
  menu db

-- Option 5
inputArtistSales :: [Album] -> IO ()
inputArtistSales db = do
  putStrLn "\nEnter an artist (case sensitive):"
  ar <- getLine
  if totalArtistSales ar db == "0"
    then do
      putStrLn "\nThis artist has no albums in the top 50"
    else do
      putStrLn
        ( "\n" ++ ar ++ " has " ++ totalArtistSales ar db
            ++ " album sales from the top 50 list\n"
        )
  menu db

-- Option 6
inputAlbumsPerArist :: [Album] -> IO ()
inputAlbumsPerArist db = do
  putStrLn ("\n" ++ artistsToString (countArtists db))
  menu db

-- Option 7
inputNewEntry :: [Album] -> IO ()
inputNewEntry db = do
  putStrLn "\nEnter the title:"
  ti <- getLine
  if length ti > 38
    then do
      putStrLn "\nThis title is too long"
      menu db
    else do
      putStrLn "\nEnter the artist:"
      ar <- getLine
      if length ar > 18
        then do
          putStrLn "\nThis artist name is too long"
          menu db
        else do
          putStrLn "\nEnter the year of release (between 1000 and 2020):"
          yrStr <- getLine
          let yr = read yrStr :: Int
          if yr > 999 && yr <= 2020
            then do
              putStrLn "\nEnter the sales:"
              slStr <- getLine
              let sl = read slStr :: Int
              if sl > 1
                then do
                  let newDb = searchListForAlbum Album {title = ti, artist = ar, year = yr, sales = sl} db
                  putStrLn ("\n" ++ albumsToString newDb)
                  menu newDb
                else do
                  putStrLn "\nNot a valid amount of sales"
                  menu db
            else do
              putStrLn "\nNot a valid year of release"
              menu db

-- Option 8
inputIncreaseAlbumSales :: [Album] -> IO ()
inputIncreaseAlbumSales db = do
  putStrLn "\nEnter the title:"
  ti <- getLine
  putStrLn "\nEnter the artist:"
  ar <- getLine
  if isNothing (findAlbum ti ar db)
    then do
      putStrLn "\nNo such album exists for the specified artist"
      menu db
    else do
      putStrLn "\nEnter the sales increase:"
      slStr <- getLine
      let sl = read slStr :: Int
      if sl > 0
        then do
          let i = findAlbum ti ar db
              newDb = increaseAlbumSales ti ar sl db
          putStrLn "\nThe sales have been added"
          putStrLn ("\n" ++ albumsToString newDb)
          menu newDb
        else do
          putStrLn "\nThe sales can't be less than 1"
          menu db

-- Option 9
writeToDatabase :: Show a => a -> IO ()
writeToDatabase db = do
  putStrLn "\nText file updating..."
  writeFile "albums.txt" (show db)
  putStrLn "Done\n"