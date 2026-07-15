-- 02
unique :: [Char] -> [Char]
unique  = uniqueHelper [] 

uniqueHelper :: [Char] -> [Char] -> [Char]
uniqueHelper _ [] = []
uniqueHelper seen (x:xs)
    | x `elem` seen = uniqueHelper seen xs
    | otherwise = x : uniqueHelper (x:seen) xs