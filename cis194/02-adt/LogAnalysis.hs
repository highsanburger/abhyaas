{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 

import Log

-- Exercise 1

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage 
parseMessage str | w == "I" = LogMessage Info (read $ v) (unwords $ ws)
                 | w == "W" = LogMessage Warning (read $ v) (unwords $ ws)
                 | w == "E" = LogMessage (Error (read $ v)) (read $ head ws) (unwords $ tail ws)
                 | otherwise = Unknown str
                    where (w:v:ws) = words str

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file



-- Exercise 2
