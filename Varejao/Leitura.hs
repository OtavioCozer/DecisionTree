module Leitura
(leEntrada
) where

leEntrada :: String -> [[String]]
leEntrada = map words . lines

