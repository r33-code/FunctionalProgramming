module Starman where
{-|
Module      : starman
Description : A guess character game
Copyright   : (c) @r33-code, 2020
License     : MIT
Maintainer  : r33ngin33r@gmail.com
Stability   : experimental
Portability : POSIX

A guess character @game@.

-}

import System.IO
import System.Random

{-| 
    check function
-}
check :: String -> String -> Char -> (Bool, String)
check word display c
    = (c `elem` word,[
        if x == c
            then c
            else y | (x,y) <- zip word display
    ])

{-| 
    turn function
-}
turn :: String -> String -> Int -> IO()
turn word display n =
    do
        if n == 0
            then putStrLn "You lose"
            else if word == display
                    then putStrLn "You win!"
                    else mkguess word display n

{-|
    mkguess function
-}
mkguess :: String -> [Char] -> Int -> IO()
mkguess word display n =
    do
        putStrLn (display ++ "  " ++ take n (repeat '*'))
        putStrLn " Enter your guess: "
        q <- getLine
        let (correct, display') = check word display (q !! 0)
        let n' = if correct then n else n-1
        turn word display' n'

{-
    splitByChar function
-}
splitByChar :: String -> Char -> [String]
splitByChar "" _ = []
splitByChar xxs@(x:xs) c 
       | x == c      = splitByChar xs c
       | otherwise   = fst(break (== c) xxs) : splitByChar (snd(break (== c) xxs)) c


{-|
    starman function

    The word is selected randomly from a dictionary text file and
    the number of attempts is selected randomly according the lenght of the chosen word.
-}
starman :: String -> Int -> IO()
starman word n = turn word ['-' | x <- word] n    


main :: IO ()
main = do 
       hFile <- openFile "dict.txt" ReadMode
       contents <- hGetContents hFile
       let lines = splitByChar contents '\n'
       randonLine <- randomRIO(0,(length lines)-1)
       let line = lines !! randonLine
       attempts <- randomRIO(0,(length line)-1) 
       starman line attempts
       hClose hFile      
