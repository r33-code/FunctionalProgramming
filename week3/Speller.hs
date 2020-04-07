module Speller where
{-|
Module      : speller
Description : Simple text generator for a spelling book
Copyright   : (c) @r33-code, 2020
License     : MIT
Maintainer  : Jullyano Lino(jullyanolino@gmail.com)
Stability   : experimental
Portability : POSIX

A simple text generator for a spelling book. 
University of Glasgow - Functional Programming
-}

{-|
    The set of the letters
-}
letters :: [Char]
letters = ['A'..'Z'] ++ ['a'..'z']

{- |
    The 'genPhrase' function generates a string indexed by its first letter.
    It takes one argument of  type [Char]
-}
genPhrase :: [Char] -> [Char]
genPhrase word = head(word) : " is for " ++ word

{- |
    The 'gentext' function generates a list of strings that are submitted to the 'genPhrase' function.
    It takes one argument of  type [[Char]]
-}
genText :: [[Char]] -> [[Char]]
genText [] = []
genText words = (map (genPhrase) words)

{- |
    The 'genSpelling' function generates a string joined from a list of some others submitted to a compiling process.
    It takes one argument of  type [[Char]]
-}
genSpelling :: [[Char]] -> [Char]
genSpelling words = gen(init(words)) ++ ", and " ++ (last(words))
    where 
        gen [] = []
        gen (x:[]) = x
        gen w = gen (init(w)) ++ ", " ++ (last(w))  

{-|
    The 'speller' function generates a string coming from the composed application of the genText and genSpelling to a list of strings.
    It takes one argument of  type [[Char]]
-}
speller :: [[Char]] -> [Char]
speller words = genSpelling $ genText words


main :: IO ()
main = do
    let spelled = speller ["Hi","I","am","the","Goku","!"]
    putStrLn spelled