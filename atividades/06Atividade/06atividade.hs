atividade = "06"
nome = "Maria Eduarda Lima Mota"

matricula = "556276"


-- 01
-- crie uma função que determine se uma string é anagrama de outra
ehLetra :: Char -> Bool
ehLetra c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') 

praMin :: Char -> Char
praMin c 
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

limparString :: [Char] -> [Char]
limparString = map praMin . filter ehLetra

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)
  where
    inserir y [] = [y]
    inserir y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : inserir y zs


anagrama :: [Char] -> [Char] -> Bool 
anagrama s1 s2 = ordenar (limparString s1) == ordenar (limparString s2)


-- 02
unique :: [Char] -> [Char]
unique  = uniqueHelper [] 

uniqueHelper :: [Char] -> [Char] -> [Char]
uniqueHelper _ [] = []
uniqueHelper seen (x:xs)
    | x `elem` seen = uniqueHelper seen xs
    | otherwise = x : uniqueHelper (x:seen) xs

-- 03
intersec :: [Char] -> [Char] -> [Char] 
intersec a b = removeDuplicates [ x|x <- a, x `elem` b]

removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


-- 04
zip'linha :: [a] -> [b] -> [c] -> [(a,b,c)] 
zip'linha (x:xs) (y:ys) (z:zs) = (x, y, z) : zip'linha xs ys zs
zip'linha _ _ _ = []
