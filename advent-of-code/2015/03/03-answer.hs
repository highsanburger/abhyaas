origin = [(0,0)]

path :: [(Int,Int)] -> [Char] -> [(Int,Int)]
path path [] = path
path ((x,y):s) ('^':cs) = path ((x,y+1):(x,y):s) cs
path ((x,y):s) ('v':cs) = path ((x,y-1):(x,y):s) cs
path ((x,y):s) ('>':cs) = path ((x+1,y):(x,y):s) cs
path ((x,y):s) ('<':cs) = path ((x-1,y):(x,y):s) cs

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

fork :: [a] -> ([a],[a])
fork [] = ([],[])
fork (a:b:s) = ((a:at),(b:bt))
             where (at,bt) = fork s  -- works only for even length

main = do
    input <- readFile "03-input.txt"
    print $ length $ rmdups $ path origin input
    print $ length $ rmdups $ path origin (fst $ fork input) ++ path origin (snd $ fork input)
