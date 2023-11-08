parseDim :: String -> [Int]
parseDim "" =           []
parseDim str = (read $ takeWhile (/= 'x') str :: Int) : parseDim s
  where
    s = dropWhile (== 'x') $ dropWhile (/= 'x') str

area :: [Int] -> Int
area [l, w, h] = 2 * l * w + 2 * w * h + 2 * h * l + minimum [l * w, w * h, h * l]

parseInputA :: String -> Int
parseInputA input = sum $ map area $ map parseDim $ words input

-- >>> removeMax [3,4,1]
-- Prelude.head: empty list
removeMax :: [Int] -> [Int] -- kinda ad-hoc
removeMax [a, b, c]
  | a == maximum [a, b, c] = [b, c]
  | b == maximum [a, b, c] = [a, c]
  | c == maximum [a, b, c] = [a, b]

ribbon :: [Int] -> Int
ribbon [l, w, h] = l * w * h + ((*) 2 $ sum $ removeMax [l, w, h])

parseInputB :: String -> Int
parseInputB input = sum $ map ribbon $ map parseDim $ words input

main = do
  input <- readFile "02-input.txt"
  print $ parseInputA input
  print $ parseInputB input
