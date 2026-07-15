
nome = "Maria Edurda Lima Mota"
matricula = "556276"
atividade = "atividade 8"


-- Árvore Binária de busca (BST)
--data BST a = Null
  --      |NodeLeft a (BST a)
    --    |NodeRight a (BST a)
      --  deriving (Show, Eq)


-- Definição do tipo BST
data BST a = Null
           | Node a (BST a) (BST a)
           deriving (Show, Eq)


-- questão 1.
parentize :: BST Int -> String
parentize Null = ""
parentize (Node val Null Null) = "(" ++ show val ++ ")"
parentize (Node val left right) = 
    "(" ++ show val ++ " " ++ parentize left ++ " " ++ parentize right ++ ")"


 -- Função para inserir um elemento na árvore BST
inserir :: Int -> BST Int -> BST Int
inserir x Null = Node x Null Null
inserir x (Node val left right)
    | x < val   = Node val (inserir x left) right
    | x > val   = Node val left (inserir x right)
    | otherwise = Node val left right 


-- questão 2.
insert :: Int -> BST Int -> BST Int
insert x Null = Node x Null Null
insert x (Node val left right)
    | x < val   = Node val (insert x left) right
    | x > val   = Node val left (insert x right)
    | otherwise = Node val left right 

addFromVec :: [Int] -> BST Int
addFromVec = foldr insert Null


-- questão 3.

height :: BST a -> Int
height Null = 0
height (Node _ left right) = 1 + max (height left) (height right)
