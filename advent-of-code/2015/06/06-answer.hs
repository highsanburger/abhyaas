type Instruction =  (Int,Int,String,Int,Int)
type Grid = [(Int,Int,Bool)]

grid = [(x,y,False) | x <- [0..999], y <- [0..999] ] :: Grid

stringToInt :: String -> Int
stringToInt str = read str :: Int

toNum :: String -> (Int,Int)
toNum n = (stringToInt $ takeWhile (/= ',') n, stringToInt $ tail $ dropWhile (/= ',') n)

toInst :: String -> Instruction
toInst str | head w == "toggle" = (fst $ toNum $ w !! 1,
                                   snd $ toNum $ w !! 1,
                                   w !! 0,
                                   fst $ toNum $ w !! 3,
                                   snd $ toNum $ w !! 3)
           | otherwise = (fst $ toNum $ w !! 2,
                          snd $ toNum $ w !! 2,
                          w !! 1,
                          fst $ toNum $ w !! 4,
                          snd $ toNum $ w !! 4)


           where w = words str

turn ::  Grid -> Instruction -> Grid
turn grid (fx,fy,"on",tx,ty) = (map (\(x,y,z) -> (x,y,True)) $
                               filter (\(x,y,z) -> (fx <= x && x <= tx) && (fy <= y && y <= ty) ) grid)
                               ++ filter (\(x,y,z) -> (fx > x || x  > tx) || (fy > y || y > ty) ) grid 

turn grid (fx,fy,"off",tx,ty) = (map (\(x,y,z) -> (x,y,False)) $
                               filter (\(x,y,z) -> (fx <= x && x <= tx) && (fy <= y && y <= ty) ) grid)
                               ++ filter (\(x,y,z) -> (fx > x || x  > tx) || (fy > y || y > ty) ) grid 
turn grid (fx,fy,"toggle",tx,ty) = (map (\(x,y,z) -> (x,y,not z)) $
                               filter (\(x,y,z) -> (fx <= x && x <= tx) && (fy <= y && y <= ty) ) grid)
                               ++ filter (\(x,y,z) -> (fx > x || x  > tx) || (fy > y || y > ty) ) grid 

countOn :: Grid -> Int
countOn g = length $ filter (\(x,y,z) -> z == True) g

lights :: Grid -> [Instruction] -> Grid
lights grid [] = grid
lights grid (i:is) = lights (turn grid i) is

x1 = "turn on 0,0 through 999,999"
x2 = "toggle 0,0 through 999,0"
x3 = "turn off 499,499 through 500,500"

i1 = toInst x1
i2 = toInst x2
i3 = toInst x3

main = do
    input <- readFile "06-input.txt"
    print $ countOn $ lights grid $ map (toInst) $ lines input
