-----------------------
-- Gabriela Truta
-- 29.11.2020
-----------------------

import Data.List (sortBy)
import Data.Function (on)

strWords :: String -> [String]
strWords stringToSplit = 
  let
    splitHelper string listOfWords word =
      case string of
        [] -> listOfWords
        x:xs -> if x /= ' ' then splitHelper xs listOfWords (x:word)
               else splitHelper xs (reverse word:listOfWords) []
  in
    splitHelper (reverse stringToSplit++" ") [] []
  
piglatinize :: String -> String
piglatinize normalString = 
  let
    listOfWords = strWords normalString
    vowels = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']
    translatePigLatin listOfWords pigLatin =
      case listOfWords of
        [] -> pigLatin
        x:xs -> if head x `elem` vowels then translatePigLatin xs (x ++ "-hay " ++ pigLatin)
                   else translatePigLatin xs (drop 1 x ++ "-" ++ [head x] ++ "ay " ++ pigLatin) 
  in
    translatePigLatin (reverse listOfWords) []

apprPi :: Double
apprPi =
  let
    nextPi :: Double -> Double
    nextPi xn = xn + (2 * cos(xn/2))/(2 * sin(xn/2) - 1)
    iter listOfWords =
      case listOfWords of
        [] -> 0
        x:xs -> if x == head xs then x
                  else iter xs
  in
    iter (iterate nextPi 0)


-- Implement:

-- the update function for option 1
update :: (Eq k) => (v -> v) -> v -> k -> [(k, v)] -> [(k, v)]
update _ _ _ _ = error "Implement this function"

-- OR

-- the uniques, countOccurrences and countWords functions for option 2
uniques :: (Eq a) => [a] -> [a]
uniques l = 
  let
    helper :: (Eq a) => [a] -> [a] -> [a]
    helper list uniquesElem = 
      case list of
        [] -> uniquesElem
        x:xs -> helper (filter (/=x) xs) [x]++uniquesElem
  in
    helper l [] 
  
countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences e l = 
  let
    helper :: (Eq a) => a -> [a] -> Int -> Int
    helper elem list count = 
      case list of
        [] -> count
        x:xs -> if elem == x then helper elem xs count+1
                  else helper elem xs count
  in
    helper e l 0

countWords :: String -> [(String, Int)]
countWords l = 
  let
    words = strWords l
    helper :: [String] -> [(String, Int)] -> [(String, Int)]
    helper list tuples = 
      case list of
        [] -> tuples
        x:xs -> helper xs [(reverse x, countOccurrences x words)]++tuples
  in
    helper (uniques words) []

-- Implement topWords using the functions implemented above.
sortTuples :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
sortTuples (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2

topWords :: Int -> String -> [(String, Int)]
topWords n listOfWords = take n (sortBy sortTuples (countWords listOfWords))