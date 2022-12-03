import Data.List

-- largestStore :: [[Int]] -> Int
largestStore stores = head (greatest capacities)
                      where capacities = map (\xs -> sum xs) stores

greatest :: [Int] -> [Int]
greatest (x:y:xs) | x < y = greatest (y:xs)
                  | x > y = greatest (x:xs)
greatest x = x

largestThreeStore stores = head x + head (tail x) + head (tail (tail x))
                            where x = reverse $ sort capacities
                                  capacities = map (\xs -> sum xs) stores

getLines :: IO [String]
getLines = do
            x <- getLine
            if x == "end" 
            then return [] else do
            xs <- getLines
            return (x:xs)

-- ["100","200","","300","400","","500"]
--groups :: [String] -> [[String]] -> [[String]]
--groups (x:"":xs) = groups xs (head (reverse acc) :x)
--groups (x:xs) acc = groups xs (acc:x)
--groups x acc = []

groups :: [String] -> [[String]]
groups xs = groupBy (\x -> \y -> y /= "") xs

removeSpace :: [String] -> [String]
removeSpace ("":xs) = removeSpace xs
removeSpace (x:xs) = x : removeSpace xs
removeSpace [] = []

main = do 
         elves <- getLines
         return (largestThreeStore (toInt (map removeSpace (groups $ elves))))

toInt :: [[String]] -> [[Int]]
toInt xs = [map (read) x | x <- xs]


