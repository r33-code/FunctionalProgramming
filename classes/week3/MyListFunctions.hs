module MyListFunctions where

import Prelude hiding (foldr, foldl)

length1' [] = 0
length1' (x:xs) = 1 + length1' xs


length2' lst=
    if lst == []
        then 0
        else let x:xs = lst in 1 + length2' xs

length3' lst
    | lst == [] = 0
    | otherwise = let x:xs = lst in 1 + length3' xs

f = f' where f' 1 = 0; f' x = x + f' (x-1)

filter' pred lst
    | null lst = []
    | otherwise = if pred x
        then x:filter pred xs
        else filter pred xs
            where x:xs = lst

map' :: Eq t => (t -> a) -> [t] -> [a]
map' f lst
    | lst == [] = []
    | otherwise = f x : map' f xs 
    where x:xs = lst

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) =  foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc lst =  foldr' f (f (last(lst)) acc) (init(lst))

{-
main :: IO ()
main = do
    --let list_mapped = []
    let list_mapped = map [1..10]
    putStrLn list_mapped
-}