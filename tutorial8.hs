-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)

module Tutorial8 where

import System.Random


-- Importing the keymap module

-- import KeymapList
import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen db = maximum [ length (fst (snd pair)) | pair <- db ]

formatLine :: Int -> (Barcode, Item) -> String
formatLine len entry = barcode ++ "..." ++ productName ++ [ '.' | i <- [1..len - (length productName) + 3]] ++ unit
    where
	barcode = fst entry
	productName = fst (snd entry)
	unit = snd (snd entry)

showCatalogue :: Catalogue -> String
showCatalogue catalogue = prettyconcat maxLength
    where
	maxLength = longestProductLen (toList catalogue)
	prettyconcat maxLength = concat [ (formatLine maxLength entry)++"\n" | entry <- (toList catalogue) ]

	
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList	(Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:xs) = Just a

-- drops all Nothings, and remove the Just constructor for other elements
catMaybes :: [Maybe a] -> [a]
catMaybes maybes = [ handleMaybe a | a <- maybes, not $ null (maybeToList a) ]
    where
	handleMaybe (Just a) = a

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems barcodes catalogue = catMaybes [get barcode catalogue | barcode <- barcodes]


-- Exercise 4
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 12

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
