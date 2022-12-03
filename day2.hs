

getLines :: IO [String]
getLines = do
            x <- getLine
            if x == "end" 
            then return [] else do
            xs <- getLines
            return (x:xs)

getPairs :: [String] -> [(String,String)]
getPairs (x:xs) = (head lineList, head $ reverse lineList) : getPairs xs 
                 where lineList = splitOnChar (==' ') x 
getPairs [] = []

splitOnChar     :: (Char -> Bool) -> String -> [String]
splitOnChar p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOnChar p s''
                            where (w, s'') = break p s'

calcScore :: [(String,String)] -> Int
calcScore (("B","X"):xs) = 0 + 1 + calcScore xs
calcScore (("B","Y"):xs) = 3 + 2 + calcScore xs
calcScore (("B","Z"):xs) = 6 + 3 + calcScore xs
calcScore (("A","X"):xs) = 0 + 3 + calcScore xs
calcScore (("A","Y"):xs) = 3 + 1 + calcScore xs
calcScore (("A","Z"):xs) = 6 + 2 + calcScore xs
calcScore (("C","X"):xs) = 0 + 2 + calcScore xs
calcScore (("C","Y"):xs) = 3 + 3 + calcScore xs
calcScore (("C","Z"):xs) = 6 + 1 + calcScore xs
calcScore [] = 0

main = do
        input <- getLines
        return $ calcScore $ getPairs $ input