module Uncertainty where

maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys) = 
    maxhelper (if x>y then x else y) ys

maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just (maxhelper x xs)

main :: IO ()
main = do
    print(show(maxhelper 2 [1,2,1]))
    print(maxfromlist [1,2,3,4,5])
    let inc = (1+)
    print(fmap inc (Just 1))
    print(fmap inc Nothing)