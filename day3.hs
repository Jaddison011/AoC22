import Data.Char
import Data.Set
-- map (filter x elem ys) xs to compare the two halfs of the lists, then do the same to the other half of the list, then translate each letter in those lists to their priorities then sum them

compareBag :: String -> Int
compareBag contents = compareCompartments xs ys
                      where xs = Prelude.take amount contents
                            ys = Prelude.drop amount contents
                            amount = (length contents) `div` 2

compareCompartments :: String -> String -> Int
compareCompartments xs ys = totalPriority diff
                            where diff = toList . fromList $ Prelude.filter (\x -> x `elem` ys) xs

totalPriority xs = sum (Prelude.map (\x -> getPriority x) xs)

getPriority x | (ord x - 90) > 0 = (ord x - 96)
              | otherwise = abs (ord x - 38)

getLines :: IO [String]
getLines = do
            x <- getLine
            if x == "end" 
            then return [] else do
            xs <- getLines
            return (x:xs)

main = do
         input <- getLines
         return $ sum $ Prelude.map (\x -> compareBag x) input