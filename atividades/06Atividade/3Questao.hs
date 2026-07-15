-- implemente uma função que determine a string formada pelos 
-- caracteres comuns a duas strins de entrada a e b. A saida não 
-- deve ter duplicadas.
intersec :: [Char] -> [Char] -> [Char] 
intersec a b = removeDuplicates [ x|x <- a, x `elem` b]

removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


-- teste
-- $ intersec "abcd" "cdef"
-- $ "cd"