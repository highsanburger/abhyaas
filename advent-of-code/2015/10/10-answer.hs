toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits ((n - (mod n 10)) `div` 10) ++ [mod n 10]


pack ::  Eq a => [a] -> [[a]]
pack [] = []
pack s = (takeWhile (== (head s)) s) : (pack (dropWhile (== (head s)) s))

encode :: [Integer] -> [[Integer]]
encode s = map (\x -> [fromIntegral $ length x, head x]) $ pack s

toNum :: [[Integer]] -> Integer
toNum ns = sum $ map (\(x,y) -> x * (10 ^ y)) $ zip n [0..]
         where n = reverse $ concat ns

looksay :: Integer -> Integer -> Integer
looksay num 1 = toNum $ encode $ toDigits num
looksay num n = looksay (looksay num (n-1)) 1


looksayLength :: Integer -> Integer -> Integer
looksayLength num n
  | n <= 0 = error "n must be a positive integer"
  | otherwise = lengthOfLooksay num n 1 0
  where
    lengthOfLooksay _ 0 _ len = len
    lengthOfLooksay numToProcess remainingCount count len
      | remainingCount <= 0 = error "n must be a positive integer"
      | remainingCount == 1 = len + 1  -- Add 1 for the last digit
      | otherwise =
        let (q, r) = numToProcess `divMod` 10
            currentDigit = r
            nextDigit = q `mod` 10
        in if currentDigit == nextDigit
           then lengthOfLooksay q (remainingCount - 1) (count + 1) len
           else lengthOfLooksay q (remainingCount - 1) 1 (len + count + 1)  -- +1 for the current digit

main = do
    input <- readFile "10-input.txt"
    print $ lines input
