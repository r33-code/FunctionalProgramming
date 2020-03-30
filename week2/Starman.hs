{-|
Module      : starman
Description : A guess character game
Copyright   : (c) @r33-code, 2020
License     : MIT
Maintainer  : r33ngin33r@gmail.com
Stability   : experimental
Portability : POSIX

A guess character @game@.

TODO: 
Generate a random word from a list of words or a dictionary file. 
It would involve generating a random number i and read in the ith word from
 a dictionary. You might import System.Random
-}

--module Starman where

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

{-|
    starman function
-}
starman :: String -> Int -> IO()
starman word n = turn word ['-' | x <- word] n

