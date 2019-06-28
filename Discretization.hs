module Discretization(
 insereValoresNumericos
)where

import Data.List

-- as funçoes abaixo determinam quai caractersiticas são numéricas--
indexColunasNumericas :: Num a1 => [[a2]] -> [a1]
indexColunasNumericas caracteristicas = indexColunasNumericas' 0 caracteristicas

indexColunasNumericas' :: Num a1 => a1 -> [[a2]] -> [a1]
indexColunasNumericas' _ [] = []
indexColunasNumericas' count (caracteristica:caracteristicas) = if ehColunaNumerica caracteristica then count : indexColunasNumericas' (count+1) caracteristicas else indexColunasNumericas' (count+1) caracteristicas 

ehColunaNumerica :: [a] -> Bool
ehColunaNumerica (a:[]) = True
ehColunaNumerica _ = False  

-- passo1 e cria colunas e juntaCOlunas são reponsável por receber uma base de dados e as caractersiticas e retornar n listas de tuplas para cada elemento da base sendo q n é a quantiddade de caracterisitcas numéricas -- 
-- e deppois juntalas em listas de tuplas de uma mamesma caracteristica--
passo1 :: [[t]] -> [[a]] -> [[(t, t)]]
passo1 ex caracteristicas =  passo1' ex (indexColunasNumericas caracteristicas)

passo1' :: [[t]] -> [Int] -> [[(t, t)]]
passo1' [] _ = []
passo1' (ex:exs) index = criaColunas ex index (last ex) : passo1' exs index

criaColunas :: [a] -> [Int] -> t -> [(a, t)]
criaColunas _ [] _ = []
criaColunas ex (index:indexs) classe = (ex!!index, classe) : criaColunas ex indexs classe 

juntaColunas :: [[t]] -> [[a]] -> [[(t, t)]]
juntaColunas ex caracteristicas = juntaColunas' (passo1 ex caracteristicas)

juntaColunas' :: [[a]] -> [[a]]
juntaColunas'  ([]:_) = []
juntaColunas' tuplas = map head tuplas : juntaColunas' (map tail tuplas)

--ordena colunas é reponsável por ordenar cada coluna de caracteristica--
ordenaColunas :: Ord a => [[(a, b)]] -> [[(a, b)]]
ordenaColunas [] = []
ordenaColunas (coluna:colunas) = (sortBy compara coluna) : ordenaColunas colunas

compara :: Ord a => (a, b1) -> (a, b2) -> Ordering
compara a b | (fst a) >= (fst b) = GT
            | (fst a) < (fst b) = LT

--determinar medianas recebe uma [(valor, classe)]--
determinaMedianas :: Eq b => [(String, b)] -> [String]
determinaMedianas (_:[]) = []
determinaMedianas (c:cs)  = if snd c == snd (head cs) then determinaMedianas cs else mediana (fst c) (fst (head cs))  : determinaMedianas cs

mediana :: String -> String -> String
mediana x y = show (((read x :: Float) + (read y :: Float)) / 2.0)

-- reecebe um base de dados e as caracteristicas e retorna a mediana das caracteristicas numericas--
determinaValoresNumericos :: [[String]] -> [[a]] -> [[String]]
determinaValoresNumericos ex caracteristicas = map determinaMedianas(ordenaColunas (juntaColunas ex caracteristicas)) 

criaIntervalos xs = criaIntervalos' xs True (head xs)

criaIntervalos' (x:xs) ehInicio inferior | ehInicio && (ehFinal (x:xs)) = ("<= " ++ x) : (">> " ++ x) : []
                                         | ehInicio == True = ("<= " ++ x) : criaIntervalos' xs False x
                                         | ehFinal (x:xs) = ("<> "++ inferior ++ " " ++ x) : [">> " ++ x]
                                         | otherwise = ("<> "++ inferior ++ " " ++ x)  : criaIntervalos' xs False x
                                         
                               
insereValoresNumericos ex caracteristicas = insereValoresNumericos' caracteristicas (map criaIntervalos(determinaValoresNumericos ex caracteristicas))

insereValoresNumericos' _ [] = []
insereValoresNumericos' (caracteristica:caracteristicas) (intervalo:intervalos) = if (ehColunaNumerica caracteristica) then (caracteristica ++ intervalo) : insereValoresNumericos' caracteristicas intervalos else caracteristica : insereValoresNumericos' caracteristicas (intervalo:intervalos)
                                             

ehFinal (_:[]) = True
ehFinal _ = False
         

y = ["22.5","24.0","26.5"]
x = ["78.5"]

teste = "<>22.5 24.0"


caracteristicas1 = [["sepalLength"],["sepalWidth"],["petalLength"],["petalWidth"]]

ex1 = [["5.1","3.5","1.4","0.2","Iris-setosa"],["4.9","3.0","1.4","0.2","Iris-setosa"],["4.7","3.2","1.3","0.2","Iris-setosa"],["4.6","3.1","1.5","0.2","Iris-setosa"],["5.0","3.6","1.4","0.2","Iris-setosa"],["5.4","3.9","1.7","0.4","Iris-setosa"],["4.6","3.4","1.4","0.3","Iris-setosa"],["5.0","3.4","1.5","0.2","Iris-setosa"],["4.4","2.9","1.4","0.2","Iris-setosa"],["4.9","3.1","1.5","0.1","Iris-setosa"],["5.4","3.7","1.5","0.2","Iris-setosa"],["4.8","3.4","1.6","0.2","Iris-setosa"],["4.8","3.0","1.4","0.1","Iris-setosa"],["4.3","3.0","1.1","0.1","Iris-setosa"],["5.8","4.0","1.2","0.2","Iris-setosa"],["5.7","4.4","1.5","0.4","Iris-setosa"],["5.4","3.9","1.3","0.4","Iris-setosa"],["5.1","3.5","1.4","0.3","Iris-setosa"],["5.7","3.8","1.7","0.3","Iris-setosa"],["5.1","3.8","1.5","0.3","Iris-setosa"],["5.4","3.4","1.7","0.2","Iris-setosa"],["5.1","3.7","1.5","0.4","Iris-setosa"],["4.6","3.6","1.0","0.2","Iris-setosa"],["5.1","3.3","1.7","0.5","Iris-setosa"],["4.8","3.4","1.9","0.2","Iris-setosa"],["5.0","3.0","1.6","0.2","Iris-setosa"],["5.0","3.4","1.6","0.4","Iris-setosa"],["5.2","3.5","1.5","0.2","Iris-setosa"],["5.2","3.4","1.4","0.2","Iris-setosa"],["4.7","3.2","1.6","0.2","Iris-setosa"],["4.8","3.1","1.6","0.2","Iris-setosa"],["5.4","3.4","1.5","0.4","Iris-setosa"],["5.2","4.1","1.5","0.1","Iris-setosa"],["5.5","4.2","1.4","0.2","Iris-setosa"],["4.9","3.1","1.5","0.1","Iris-setosa"],["5.0","3.2","1.2","0.2","Iris-setosa"],["5.5","3.5","1.3","0.2","Iris-setosa"],["4.9","3.1","1.5","0.1","Iris-setosa"],["4.4","3.0","1.3","0.2","Iris-setosa"],["5.1","3.4","1.5","0.2","Iris-setosa"],["5.0","3.5","1.3","0.3","Iris-setosa"],["4.5","2.3","1.3","0.3","Iris-setosa"],["4.4","3.2","1.3","0.2","Iris-setosa"],["5.0","3.5","1.6","0.6","Iris-setosa"],["5.1","3.8","1.9","0.4","Iris-setosa"],["4.8","3.0","1.4","0.3","Iris-setosa"],["5.1","3.8","1.6","0.2","Iris-setosa"],["4.6","3.2","1.4","0.2","Iris-setosa"],["5.3","3.7","1.5","0.2","Iris-setosa"],["5.0","3.3","1.4","0.2","Iris-setosa"],["7.0","3.2","4.7","1.4","Iris-versicolor"],["6.4","3.2","4.5","1.5","Iris-versicolor"],["6.9","3.1","4.9","1.5","Iris-versicolor"],["5.5","2.3","4.0","1.3","Iris-versicolor"],["6.5","2.8","4.6","1.5","Iris-versicolor"],["5.7","2.8","4.5","1.3","Iris-versicolor"],["6.3","3.3","4.7","1.6","Iris-versicolor"],["4.9","2.4","3.3","1.0","Iris-versicolor"],["6.6","2.9","4.6","1.3","Iris-versicolor"],["5.2","2.7","3.9","1.4","Iris-versicolor"],["5.0","2.0","3.5","1.0","Iris-versicolor"],["5.9","3.0","4.2","1.5","Iris-versicolor"],["6.0","2.2","4.0","1.0","Iris-versicolor"],["6.1","2.9","4.7","1.4","Iris-versicolor"],["5.6","2.9","3.6","1.3","Iris-versicolor"],["6.7","3.1","4.4","1.4","Iris-versicolor"],["5.6","3.0","4.5","1.5","Iris-versicolor"],["5.8","2.7","4.1","1.0","Iris-versicolor"],["6.2","2.2","4.5","1.5","Iris-versicolor"],["5.6","2.5","3.9","1.1","Iris-versicolor"],["5.9","3.2","4.8","1.8","Iris-versicolor"],["6.1","2.8","4.0","1.3","Iris-versicolor"],["6.3","2.5","4.9","1.5","Iris-versicolor"],["6.1","2.8","4.7","1.2","Iris-versicolor"],["6.4","2.9","4.3","1.3","Iris-versicolor"],["6.6","3.0","4.4","1.4","Iris-versicolor"],["6.8","2.8","4.8","1.4","Iris-versicolor"],["6.7","3.0","5.0","1.7","Iris-versicolor"],["6.0","2.9","4.5","1.5","Iris-versicolor"],["5.7","2.6","3.5","1.0","Iris-versicolor"],["5.5","2.4","3.8","1.1","Iris-versicolor"],["5.5","2.4","3.7","1.0","Iris-versicolor"],["5.8","2.7","3.9","1.2","Iris-versicolor"],["6.0","2.7","5.1","1.6","Iris-versicolor"],["5.4","3.0","4.5","1.5","Iris-versicolor"],["6.0","3.4","4.5","1.6","Iris-versicolor"],["6.7","3.1","4.7","1.5","Iris-versicolor"],["6.3","2.3","4.4","1.3","Iris-versicolor"],["5.6","3.0","4.1","1.3","Iris-versicolor"],["5.5","2.5","4.0","1.3","Iris-versicolor"],["5.5","2.6","4.4","1.2","Iris-versicolor"],["6.1","3.0","4.6","1.4","Iris-versicolor"],["5.8","2.6","4.0","1.2","Iris-versicolor"],["5.0","2.3","3.3","1.0","Iris-versicolor"],["5.6","2.7","4.2","1.3","Iris-versicolor"],["5.7","3.0","4.2","1.2","Iris-versicolor"],["5.7","2.9","4.2","1.3","Iris-versicolor"],["6.2","2.9","4.3","1.3","Iris-versicolor"],["5.1","2.5","3.0","1.1","Iris-versicolor"],["5.7","2.8","4.1","1.3","Iris-versicolor"],["6.3","3.3","6.0","2.5","Iris-virginica"],["5.8","2.7","5.1","1.9","Iris-virginica"],["7.1","3.0","5.9","2.1","Iris-virginica"],["6.3","2.9","5.6","1.8","Iris-virginica"],["6.5","3.0","5.8","2.2","Iris-virginica"],["7.6","3.0","6.6","2.1","Iris-virginica"],["4.9","2.5","4.5","1.7","Iris-virginica"],["7.3","2.9","6.3","1.8","Iris-virginica"],["6.7","2.5","5.8","1.8","Iris-virginica"],["7.2","3.6","6.1","2.5","Iris-virginica"],["6.5","3.2","5.1","2.0","Iris-virginica"],["6.4","2.7","5.3","1.9","Iris-virginica"],["6.8","3.0","5.5","2.1","Iris-virginica"],["5.7","2.5","5.0","2.0","Iris-virginica"],["5.8","2.8","5.1","2.4","Iris-virginica"],["6.4","3.2","5.3","2.3","Iris-virginica"],["6.5","3.0","5.5","1.8","Iris-virginica"],["7.7","3.8","6.7","2.2","Iris-virginica"],["7.7","2.6","6.9","2.3","Iris-virginica"],["6.0","2.2","5.0","1.5","Iris-virginica"],["6.9","3.2","5.7","2.3","Iris-virginica"],["5.6","2.8","4.9","2.0","Iris-virginica"],["7.7","2.8","6.7","2.0","Iris-virginica"],["6.3","2.7","4.9","1.8","Iris-virginica"],["6.7","3.3","5.7","2.1","Iris-virginica"],["7.2","3.2","6.0","1.8","Iris-virginica"],["6.2","2.8","4.8","1.8","Iris-virginica"],["6.1","3.0","4.9","1.8","Iris-virginica"],["6.4","2.8","5.6","2.1","Iris-virginica"],["7.2","3.0","5.8","1.6","Iris-virginica"],["7.4","2.8","6.1","1.9","Iris-virginica"],["7.9","3.8","6.4","2.0","Iris-virginica"],["6.4","2.8","5.6","2.2","Iris-virginica"],["6.3","2.8","5.1","1.5","Iris-virginica"],["6.1","2.6","5.6","1.4","Iris-virginica"],["7.7","3.0","6.1","2.3","Iris-virginica"],["6.3","3.4","5.6","2.4","Iris-virginica"],["6.4","3.1","5.5","1.8","Iris-virginica"],["6.0","3.0","4.8","1.8","Iris-virginica"],["6.9","3.1","5.4","2.1","Iris-virginica"],["6.7","3.1","5.6","2.4","Iris-virginica"],["6.9","3.1","5.1","2.3","Iris-virginica"],["5.8","2.7","5.1","1.9","Iris-virginica"],["6.8","3.2","5.9","2.3","Iris-virginica"],["6.7","3.3","5.7","2.5","Iris-virginica"],["6.7","3.0","5.2","2.3","Iris-virginica"],["6.3","2.5","5.0","1.9","Iris-virginica"],["6.5","3.0","5.2","2.0","Iris-virginica"],["6.2","3.4","5.4","2.3","Iris-virginica"],["5.9","3.0","5.1","1.8","Iris-virginica"]]

intervalos1 = [["<= 4.8500004","<> 4.8500004 4.9","<> 4.9 4.9","<> 4.9 4.95","<> 4.95 5.0","<> 5.0 5.05","<> 5.05 5.1","<> 5.1 5.1499996","<> 5.1499996 5.2","<> 5.2 5.3500004","<> 5.3500004 5.4","<> 5.4 5.45","<> 5.45 5.5","<> 5.5 5.55","<> 5.55 5.6","<> 5.6 5.6499996","<> 5.6499996 5.7","<> 5.7 5.7","<> 5.7 5.75","<> 5.75 5.8","<> 5.8 5.8","<> 5.8 5.8500004","<> 5.8500004 5.9","<> 5.9 5.95","<> 5.95 6.0","<> 6.0 6.05","<> 6.05 6.1","<> 6.1 6.1499996","<> 6.1499996 6.2","<> 6.2 6.25","<> 6.25 6.3","<> 6.3 6.3500004","<> 6.3500004 6.4","<> 6.4 6.45","<> 6.45 6.5","<> 6.5 6.6499996","<> 6.6499996 6.7","<> 6.7 6.75","<> 6.75 6.8","<> 6.8 6.8500004","<> 6.8500004 6.9","<> 6.9 7.05",">> 7.05"],["<= 2.1","<> 2.1 2.2","<> 2.2 2.3","<> 2.3 2.35","<> 2.35 2.45","<> 2.45 2.5","<> 2.5 2.55","<> 2.55 2.6","<> 2.6 2.65","<> 2.65 2.7","<> 2.7 2.75","<> 2.75 2.8","<> 2.8 2.85","<> 2.85 2.9","<> 2.9 2.9","<> 2.9 2.95","<> 2.95 3.0","<> 3.0 3.0","<> 3.0 3.05","<> 3.05 3.1","<> 3.1 3.1","<> 3.1 3.15","<> 3.15 3.2","<> 3.2 3.2","<> 3.2 3.25","<> 3.25 3.3","<> 3.3 3.3","<> 3.3 3.35","<> 3.35 3.4","<> 3.4 3.4","<> 3.4 3.55","<> 3.55 3.6","<> 3.6 3.75","<> 3.75 3.8",">> 3.8"],["<= 2.45","<> 2.45 4.45","<> 4.45 4.5","<> 4.5 4.75","<> 4.75 4.8","<> 4.8 4.8500004","<> 4.8500004 4.9","<> 4.9 4.95","<> 4.95 5.0","<> 5.0 5.05","<> 5.05 5.1","<> 5.1 5.1499996",">> 5.1499996"],["<= 0.8","<> 0.8 1.3499999","<> 1.3499999 1.4","<> 1.4 1.45","<> 1.45 1.5","<> 1.5 1.55","<> 1.55 1.6","<> 1.6 1.6500001","<> 1.6500001 1.7","<> 1.7 1.75","<> 1.75 1.8","<> 1.8 1.8499999",">> 1.8499999"]]