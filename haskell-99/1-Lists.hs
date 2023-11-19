import Data.List

-- 1. Find the last element of a list.
-- λ> myLast [1,2,3,4] ~~~> 4  
-- λ> myLast ['x','y','z'] ~~~> 'z'

myLast :: [a] -> a
myLast (last:[]) = last
myLast (_:xs) = myLast xs


-- 2. Find the last-but-one (or second-last) element of a list.
-- λ> myButLast [1,2,3,4] ~~~> 3
-- λ> myButLast ['a'..'z'] ~~~> 'y'

myButLast :: [a] -> a
myButLast (blast:_:[]) = blast
myButLast (_:xs) = myButLast xs


-- 3. Find the K'th element of a list.
-- λ> elementAt [1,2,3] 2 ~~~> 2
-- λ> elementAt "haskell" 5 ~~~> 'e'

elementAt :: [a] -> Int -> a
elementAt (a:_) 1 = a
elementAt (_:xs) n = elementAt xs (n-1)


-- 4. Find the number of elements in a list.
-- λ> myLength [123, 456, 789] ~~~> 3
-- λ> myLength "Hello, world!" ~~~> 13

myLength :: [a] -> Int
myLength [] = 0
myLength (_:as) = 1 + myLength as


-- 5. Reverse a list.
-- λ> myReverse "A man, a plan, a canal, panama!" ~~~> "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4] ~~~> [4,3,2,1]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]


-- 6. Find out whether a list is a palindrome.
-- λ> isPalindrome [1,2,3] ~~~> False
-- λ> isPalindrome "madamimadam" ~~~> True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1] ~~~> True

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True 
isPalindrome (a:[]) = True
isPalindrome as = (head as == last as) && (isPalindrome $ tail $ init as)


-- 7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
-- λ> flatten (Elem 5) ~~~> [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) ~~~> [1,2,3,4,5]
-- λ> flatten (List []) ~~~> []

flatten :: [NestedList a] -> [a]
flatten = undefined


-- 8. Eliminate consecutive duplicates of list elements.
-- λ> compress "aaaabccaadeeee" ~~~> "abcade"

compress :: String -> String 
compress (c:[]) = [c]
compress (c:cs) | c == head cs = compress cs
                | otherwise = [c] ++ compress cs


-- 9. Pack consecutive duplicates of list elements into sublists.
-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] ~~~> ["aaaa","b","cc","aa","d","eeee"]

pack ::  Eq a => [a] -> [[a]]
pack [] = []
pack s = (takeWhile (== (head s)) s) : (pack (dropWhile (== (head s)) s))


-- 10. Run-length encoding of a list.
-- λ> encode "aaaabccaadeeee" ~~~> [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: Eq a => [a] -> [(Int, a)]
encode s = map (\x -> (length x, head x)) $ pack s


-- 11. Modified run-length encoding.
-- λ> encodeModified "aaaabccaadeeee" ~~~> [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

data Code a = Single a | Multiple Int a deriving Show

toCode :: (Int,a) -> Code a
toCode (1,x) = Single x
toCode (n,x) = Multiple n x

encodeModified :: Eq a => [a] -> [Code a]
encodeModified s = map (toCode) $ encode s


-- 12. Decode a run-length encoded list.
-- λ> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--   ~~~> "aaaabccaadeeee"

fromCode :: Code a -> [a] 
fromCode (Single x) = [x]
fromCode (Multiple n x) = replicate n x

decodeModified :: [Code a] -> [a]
decodeModified s = foldr (\x y -> fromCode x ++ y) [] s


-- 13. Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- λ> encodeDirect "aaaabccaadeeee"~~~>
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

-- TODO

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = undefined


-- 14. Duplicate the elements of a list.
-- λ> dupli [1, 2, 3]~~~> [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli [] = []
dupli (a:as) = [a,a] ++ dupli as


-- 15. Replicate the elements of a list a given number of times.
-- λ> repli "abc" 3 ~~~> "aaabbbccc"

replic :: a -> Int -> [a]
replic _ 0 = []
replic a 1 = [a]
replic a n = (a:replic a (n-1))

repli :: [a] -> Int -> [a]
repli as n = concat $ map (\x -> replic x n) as

-- 16. Drop every N'th element from a list.
-- λ> dropEvery "abcdefghik" 3 ~~~> "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery s n = map (\(e,i) -> e) $ filter (\(e,i) -> i `mod` n /= 0) $ zip s [1..]


-- 17. Split a list into two parts; the length of the first part is given.
-- λ> split "abcdefghik" 3 ~~~> ("abc", "defghik")

split :: [a] -> Int -> ([a],[a])
split s n = ( map (\(e,i) -> e) $ takeWhile (\(e,i) -> i < n ) l, map (\(e,i) -> e) $ dropWhile (\(e,i) -> i < n ) l)
           where l = zip s [0..]


-- 18. Extract a slice from a list.
-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 ~~~> "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice as from to = map (\(e, i) -> e) $ filter (\(e, i) -> from <= i && i <= to) l
                  where l = zip as [1..]


-- 19. rotate a list n places to the left.
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3 ~~~> "defghabc"
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2) ~~~> "ghabcdef"

rotate :: Ord a => [a] -> Int -> [a]
rotate as n = map ( \(i,e) ->  as !! ( (i + n) `mod` (length as) ) ) $ zip [0..] as
 

-- 20. Remove the K'th element from a list.
-- λ> removeAt 2 "abcd" ~~~> ('b',"acd")

removeAt :: Int -> [a] -> (a,[a])
removeAt n as = (as !! (n-1) , map (\(e,i) -> e) $ filter (\(e,i) -> i /= n) l)
               where l = zip as [1..]


-- 21. Insert an element at a given position into a list.
-- λ> insertAt 'X' "abcd" 2  ~~~> "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt a as n = fst s ++ [a] ++ snd s
                 where s = split as (n - 1)


-- 22. Create a list containing all integers within a given range.
-- λ> range 4 9 ~~~> [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range n m     |  n == m  =  [n]
              |  otherwise = (n : range (n + 1) m)


-- 23. Extract a given number of randomly selected elements from a list.
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn ~~~> eda


-- 24. Lotto: Draw N different random numbers from the set 1..M.
-- λ> diff_select 6 49 ~~~> [23,1,17,33,21,37]


-- 25. Generate a random permutation of the elements of a list.
-- λ> rnd_permu "abcdef" ~~~> "badcef"


-- 26. (**) Generate combinations of K distinct objects chosen from the N elements of a list.
-- λ> combinations 3 "abcdef" ~~~> ["abc","abd","abe",...]


-- 27. Group the elements of a set into disjoint subsets.
-- λ> group [2,3,4] ~~~>
-- ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] 
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

-- λ> group [2,2,5] ~~~> 
-- ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)


-- 28. Sorting a list of lists according to length of sublists.
-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"] ~~~> ["o","de","de","mn","abc","fgh","ijkl"]
-- λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] ~~~> ["ijkl","o","abc","fgh","de","de","mn"]
