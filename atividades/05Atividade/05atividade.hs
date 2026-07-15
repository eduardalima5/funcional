
atividade =  "05"
nome = "Maria Eduarda Lima Mota"
matricula = "556276"

--1
bis :: Int -> Bool
bis ano
    | ano `mod` 400 == 0 = True
    | ano `mod` 100 == 0 = False 
    | ano `mod` 4 == 0   = True
    | otherwise            = False


--2
temp :: Float -> Char -> Char -> Float
temp t fr to 
    |fr == 'C' && to == 'F' = celsiusToFehrenheit t
    |fr == 'C' && to == 'K' = celsiusToKelvin t
    |fr == 'F' && to == 'C' = fehrenheitToCelsius t
    |fr == 'F' && to == 'K' = fehrenheitToKelvin t
    |fr == 'K' && to == 'C' = kelvinToCelsius t
    |fr == 'K' && to == 'F' = kelvinToFehrenheit t
    |fr == to = t

celsiusToFehrenheit :: Float -> Float
celsiusToFehrenheit valor = valor * (9/5) + 32

celsiusToKelvin :: Float -> Float
celsiusToKelvin valor = valor + 273

fehrenheitToCelsius :: Float -> Float
fehrenheitToCelsius valor = (valor - 32) * (5/9)

fehrenheitToKelvin :: Float -> Float
fehrenheitToKelvin valor = (valor - 32) * (5/9) + 273

kelvinToCelsius :: Float -> Float
kelvinToCelsius valor = valor - 273

kelvinToFehrenheit :: Float -> Float 
kelvinToFehrenheit valor = (valor - 273) * (9/5) + 32


--3
