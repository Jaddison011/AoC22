getLines :: IO [String]
getLines = do
            x <- getLine
            if x == "end" 
            then return [] else do
            xs <- getLines
            return (x:xs)

splitOnChar     :: (Char -> Bool) -> String -> [String]
splitOnChar p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOnChar p s''
                            where (w, s'') = break p s'

removeSpace :: [String] -> [String]
removeSpace ("":xs) = removeSpace xs
removeSpace (x:xs) = x : removeSpace xs
removeSpace [] = []

getPairs :: String -> ([Int],[Int])
getPairs line = ([(read (head elf1range) :: Int) .. (read (last elf1range) :: Int)], [(read (head elf2range) :: Int) .. (read (last elf2range) :: Int)]) 
                where splitLine = removeSpace $ splitOnChar (==',') line
                      elf1 = head splitLine
                      elf2 = head $ reverse splitLine
                      elf1range = removeSpace $ splitOnChar (=='-') elf1
                      elf2range = removeSpace $ splitOnChar (=='-') elf2

comparePairs :: ([Int],[Int]) -> Int
comparePairs (e1,e2) | or ((or $ map (\x -> x `elem` e2) e1):(or $ map (\x -> x `elem` e1) e2):[]) = 1
                     | otherwise = 0

main = do
         lines <- getLines
         let pairs = map (\x -> getPairs x) lines
         let compared = map (\x -> comparePairs x) pairs
         return $ sum compared
         --return pairs