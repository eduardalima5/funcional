atividade = "3"

nome = "Maria Eduarda Lima Mota"
matricula = "556276"

tls :: String -> [(Char, Int)]
tls (x:xs) = contar x (x:xs) : tls (filter(/=x) xs)
            where
            contar c str = (c, length(filter (==c) str))

sfq :: String -> [(String, Int)]
sfq str = contarPalavra (words str)
  where
    contarPalavra (x:xs) = (x, contar x (words str)) : contarPalavra (filter (/= x) xs)
      where
        contar palavra = length.filter (== palavra)