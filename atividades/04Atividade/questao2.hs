
-- Função principal que ordena a lista usando o algoritmo de ordenação por inserção
insertionSort :: Ord a => [a] -> [a]
insertionSort  = foldr insertInOrder [] 

insertInOrder :: Ord a => a -> [a] -> [a]
insertInOrder x [] = [x]
insertInOrder x (y:ys)
    | x >= y    = x : y : ys
    | otherwise = y : insertInOrder x ys