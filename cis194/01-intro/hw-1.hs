{- Credit Card Numbers -}

-- Ex 1

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits ((n - (mod n 10)) `div` 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = [mod n 10] ++ toDigitsRev ((n - (mod n 10)) `div` 10) 


-- Ex 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = reverse $ map (\(ind,el) -> if (mod ind 2 == 0) then el * 2 else el) $ zip [1..] $ reverse ns


-- Ex 3

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ concat $ map toDigits ns


-- Ex 4 

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0


{- Towers of Hanoi -}

type Peg = String
type Move = (Peg, Peg)

-- Ex 5

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1,p2)]                                                   -- p1 => start, p2 => end
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1,p2)] ++ hanoi (n-1) p3 p2 p1 


-- Ex 6

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 p1 p2 p3 p4 = [(p1,p4)]                            -- start => p1 , end => p4
hanoi' n p1 p2 p3 p4 = undefined


