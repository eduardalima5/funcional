atividade = "2"

nome = "Maria Eduarda Lima Mota"
matricula = "556276"

-- 1 
-- Sejam as tuplas u e v de inteiros
-- tal que exista um inteiro k onde
-- u = kv ou v = ku  
-- então u e v são mútiplos. Construa 
-- função que determine se duas 
-- tuplas de inteiros  são múltiplas.
isMult :: (Int, Int) -> (Int, Int) -> Bool
isMult (u1, u2) (v1, v2) = mod u1 v2 == 0 && mod u2 v2 == 0 || mod v1 u1 == 0 && mod v2 u2 == 0

-- 2
-- Sejam todos os triângulos retângulos
-- de perímetro p e de lados inteiros.
--   representados por tuplas (a,b,c) 
-- com  a>=b>=c. Criar  
--  função que determine 
-- o total destes triângulos dado p .
tot'tri  :: Int -> Int
tot'tri p = length [(a, b, c) | a <- [1..(div p 2)], b <- [1..a], let c = p - a - b, a*a == (b*b) + (c*c)]