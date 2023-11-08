import Data.List


celate :: Char -> Int
celate '(' = 1
celate ')' = -1

whatFloor :: String -> Int
whatFloor str = sum $ map celate str

basement :: String -> Maybe Int
basement str = elemIndex (-1) $ scanl (\a b -> a + celate b) 0 str


main = do
    input <- readFile "01-input.txt"
    print $ whatFloor input
    print $ basement input

