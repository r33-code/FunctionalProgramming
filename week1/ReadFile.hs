import System.IO
import System.Random

{-
source:http://www.codecodex.com/wiki/Count_the_number_of_occurrences_of_a_specific_character_in_a_string#Haskell
-}
countOfChar :: Char -> [Char] -> Int
countOfChar c str = length $ filter (\x -> x == c) str

{-
-}
splitByChar :: String -> Char -> [String]
splitByChar "" _ = []
splitByChar xxs@(x:xs) c 
       | x == c      = splitByChar xs c
       | otherwise   = fst(break (== c) xxs) : splitByChar (snd(break (== c) xxs)) c

main :: IO ()
main = do 
       hFile <- openFile "dict.txt" ReadMode
       contents <- hGetContents hFile
--       let lines = words contents
       let lines = splitByChar contents '\n'
       print lines
       randN <- randomRIO(0,(length lines)-1)
       print (lines !! randN)
       hClose hFile      
