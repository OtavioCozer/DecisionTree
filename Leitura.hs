module Leitura
(leEntrada
) where

--AO INICIAR O TRABALHO EU ACHEI Q TERIA MAIS FUNÇOES AQUI

-- usada para transformar a string de entrada para a forma desejada
leEntrada :: String -> [[String]]
leEntrada = map words . lines

