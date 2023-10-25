x1 = "qjhvhtzxzqqjkmpb"
x2 = "xxyxx"
x3 = "uurcxstgmygtbstg"
x4 = "ieodomkazucvgmuy"

vowels = "aeiou"
letters = "abcdefghijklmnopqrstuvwxyz"
banned = ["ab","cd","pq","xy"]

hasVowels :: String -> Bool
hasVowels str = (length $ filter (\c -> elem c vowels) str ) >= 3

doublify :: String -> [String]      -- "abc" ~~~> ["ab", "bc"]
doublify (c:d:[]) = [(c:d:[])]
doublify (a:b:c) = (a:b:[]): doublify (b:c)

present :: Eq a => [a] -> [a] -> Bool
present v w = or [x == y | x <- v, y <- w]

hasDouble :: String -> Bool
hasDouble str = present (doublify str) (map (\x -> [x,x]) letters)

hasBanned :: String -> Bool
hasBanned str = present (doublify str) banned

isNice :: String -> Bool
isNice str = (hasVowels str) && (hasDouble str) && (not $ hasBanned str)

-- twicify :: String -> [String]
-- twicify (c:d:[]) = [(c:d:[])]
-- twicify (a:b:c) = (a:b:[]) : twicify c


twicify :: String -> [(Char, Char)]
twicify [] = []
twicify [_] = []
twicify (x:y:xs) = (x, y) : twicify (y:xs)

-- hasTwice :: [String] -> Bool
-- hasTwice [] = False
-- hasTwice (x:xs) = (any (\l -> l == x) xs) || hasTwice xs

hasTwice :: [(Char, Char)] -> Bool
hasTwice [] = False
hasTwice (x:xs) = elem x xs || hasTwice xs

-- hasThrice :: String -> Bool 
-- hasThrice [] = False
-- hasThrice (a:[]) = False
-- hasThrice (a:b:[]) = False
-- hasThrice (a:b:c:s) = (a == c) || hasThrice (b:c:s)

hasThrice :: String -> Bool
hasThrice [] = False
hasThrice (_:[]) = False
hasThrice (a:b:c:s) = a == c || hasThrice (b:c:s)

isNice2 :: String -> Bool
isNice2 str = (hasTwice $ twicify str) && (hasThrice str)

main = do
    input <- readFile "input5.txt"
    print $ length $ filter isNice2 $ lines input
    print $ length $ filter isNice $ lines input
    -- print $ length $ map (x -> length x) $ lines input
