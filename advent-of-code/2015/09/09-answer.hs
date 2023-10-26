s = "Tristram to AlphaCentauri = 34\nTristram to Snowdin = 100\nTristram to Tambi = 63\nTristram to Faerun = 108\nTristram to Norrath = 111\nTristram to Stra ylight = 89\nTristram to Arbre = 132\nAlphaCentauri to Snowdin = 4\nAlphaCentauri to Tambi = 79\nAlphaCentauri to Faerun = 44\nAlphaCentauri to Norrath = 147\nAlphaCentauri to Straylight = 133\nAlphaCentauri to Arbre = 74\nSnowdin to Tambi = 105\nSnowdin to Faerun = 95\nSnowdin to Norrath = 48\nSnowdin to Straylight = 88\nSnowdin to Arbre = 7\nTambi to Faerun = 68\nTambi to Norrath = 134\nTambi to Straylight = 107\nTambi to Arbre = 40\nFaerun to Norrath = 11\nFaerun to Straylight = 66\nFaerun to Arbre = 144\nNorrath to Straylight = 115\nNorrath to Arbre = 135\nStraylight to Arbre = 127\n"

-- stringToInt :: String -> Int
-- stringToInt s = read s 
--
-- pretty :: String -> [(String,String,Int)]
-- pretty str = [(words l !! 0, words l !! 2,stringToInt $ words l !! 4) | l <- lines str]

newtype From = String 
newtype To = String 
data Path = From To Int

main = do
    input <- readFile "09-input.txt"
    print $ input
