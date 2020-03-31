import System.IO



splitBy :: [Char] -> [String]



main :: IO ()
main = do 
       hFile <- openFile "test.txt" ReadMode
       contents <- hGetContents hFile
       lines <- splitOn "\n" contents
       putStrLn $ "Number of lines: " ++ (show (length lines))
       hClose hFile      

