--Generation of a list of an integer factors
properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

--Counting how many factors there are for x
numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

--Selecting integers that have no factors between 2 and itself minus one inclusive
primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..]


main :: IO ()
main = do
    --Generation os list of fibonacci terms
    let fibs = 1:1:(zipWith (+) fibs (tail fibs))

    --Generation of a infinite list of successive factorial numbers
    let facts = map (\x -> (fold (*) 1 [1..x])) [1..]