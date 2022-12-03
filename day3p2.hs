import Data.Char
import Data.Set
import Data.List

-- map (filter x elem ys) xs to compare the two halfs of the lists, then do the same to the other half of the list, then translate each letter in those lists to their priorities then sum them

-- compareCompartments :: String -> String -> String -> Int
compareCompartments xs ys zs =  totalPriority $ diff (diff xs ys) zs

diff xs ys = toList . fromList $ Prelude.filter (\x -> x `elem` ys) xs

totalPriority xs = sum (Prelude.map (\x -> getPriority x) xs)

getPriority x | isUpper x = abs (ord x - 38)
              | otherwise = (ord x - 96)

getLines :: IO [String]
getLines = do
            x <- getLine
            if x == "end" 
            then return [] else do
            xs <- getLines
            return (x:xs)

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n l
  | n > 0 = (Prelude.take n l) : (groups n (Prelude.drop n l))
  | otherwise = error "Negative or zero n"

main = do
        input <- getLines
        let grouped = groups 3 input
        -- return grouped
        -- return $ Prelude.map (\x -> compareCompartments (head x) (head $ tail x) (head $ reverse x)) grouped
        return $ sum $ Prelude.map (\x -> compareCompartments (head x) (head $ tail x) (head $ reverse x)) grouped
