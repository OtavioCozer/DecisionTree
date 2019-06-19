module Func(
 entropia',
 entropia,
 frequency,
 percentagens,
 percentagens'
)where

import Data.List    

-- Entropia e entropia' calcula a entropia da base de exemplos--

entropia :: (Eq a) => [a] -> Float
entropia x = entropia' (percentagens x)

entropia' :: [Float] -> Float
entropia' x = -(sum $ map (\z -> z*(logBase 2 z) ) x)

-----------------------------------------------------------------

--As funções à seguir são usadas parar calcular a percentagens de ocorrência--

frequency :: Eq a => a -> [a] -> Int
frequency x xs =   (length . filter (==x)) xs

percentagens :: (Fractional a1, Eq a2) => [a2] -> [a1]
percentagens xs = percentagens' (nub xs) xs (length xs)

percentagens' :: (Fractional a1, Integral t, Eq a2) => [a2] -> [a2] -> t -> [a1]
percentagens' [] _ _ = []
percentagens' (x:xs) list n = ( fromIntegral (frequency x list)) / (fromIntegral n)  : (percentagens' xs list n)

-------------------------------------------------------------------------------------------------------------------

-- a funcção a seguir implementa a seguinte definição {x E Ex/ values(x,a)=v} mostrada no enunciado do trabalho--
-- ela deve passar a sua base de dados e um valor de uma dada caracteristica

definition1 :: [[String]] -> String -> [String]
definition1 ex value = [ (head x) | x <- ex, (head x) == value ] 

definition2 ex caracteristica = definition2' ex (tail caracteristica)

definition2' ex values = [ (fromIntegral (length def1)) * (entropia def1) / (fromIntegral (length ex)) | v <- values , def1 <- definition1 ex v ]   
---------------------------------------------------------------------------

-- a função abaixo é usada para calcular IG definido no enunciado a primeira caracteristica de ex deve ser a--

ig ex caracteristica = (entropia ex) - sum (definition2 ex caracteristica)

--a função abaixo calcula IV com as mesmas condiçoes de IG--

iv ex caracteristica = []

dado = ["Aparencia","Sol","Chuva","Nublado"]
base = [["Sol","25","72","Sim","Va"],["Sol","28","91","Sim","NaoVa"],["Sol","22","70","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Sol","30","85","Nao","NaoVa"]]

