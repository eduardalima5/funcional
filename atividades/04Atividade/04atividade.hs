atividade = "04"

nome = "Maria Eduarda Lima Mota"
matricula = "556276"


-- 1- Substitua na string "text" todas as aparições de "from" por "to" . Retorna a string de entrada com as devidas  substituições.
replace :: [Char] -> [Char] -> [Char] -> [Char]
replace [] _ _ = []  
replace text from to
    | from `prefixo` text = to ++ replace (drop (length from) text) from to
    | otherwise = head text : replace (tail text) from to
    where prefixo :: String -> String -> Bool
          prefixo [] _ = True
          prefixo _ [] = False
          prefixo (x:xs) (y:ys) = x == y && prefixo xs ys

-- 2 - identifica o maior valor n de uma lista de inteiros L e retorna a tupla (A,n,B) onde A é a sublista de L dos valores que precedem n e B a sublista de L que sucedem n.
-- exemplo: lsSplit [1, 7, 11, 5, -2]
-- ([1,7], 11, [5, -2])

lsSplit :: [Int] -> ([Int], Int, [Int])
lsSplit [] = error "Lista vazia"
lsSplit [_] = error "Lista com apenas um elemento"
lsSplit xs =
    let maxNum = maximum xs
        (antes, n:depois) = span (/= maxNum) xs
    in (antes, n, depois)


-- 3 - Ordena do maior para o menor  uma lista usando o algoritmo de ordenação por seleção.
insertionSort :: Ord a => [a] -> [a]
insertionSort  = foldr insertInOrder [] 

insertInOrder :: Ord a => a -> [a] -> [a]
insertInOrder x [] = [x]
insertInOrder x (y:ys)
    | x >= y    = x : y : ys
    | otherwise = y : insertInOrder x ys