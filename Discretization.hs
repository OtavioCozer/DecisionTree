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
passo1 ex caracteristicas = passo1' ex (indexColunasNumericas caracteristicas)

passo1' :: [[t]] -> [Int] -> [[(t, t)]]
passo1' [] _ = []
passo1' (ex:exs) index =  criaColunas ex index (last ex) : passo1' exs index

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

insereValoresNumericos' (caracteristica:caracteristicas) [] = (caracteristica:caracteristicas)
insereValoresNumericos' (caracteristica:caracteristicas) (intervalo:intervalos) = if (ehColunaNumerica caracteristica) then (caracteristica ++ intervalo) : insereValoresNumericos' caracteristicas intervalos else caracteristica : insereValoresNumericos' caracteristicas (intervalo:intervalos)
                                             

ehFinal (_:[]) = True
ehFinal _ = False
         

y = ["22.5","24.0","26.5"]
x = ["78.5"]

teste = "<>22.5 24.0"


caracteristicas1 = [["Aparencia","Sol","Chuva","Nublado"],["Temperatura","<= 22.5","<> 22.5 24.0","<> 24.0 26.5",">> 26.5"],["Umidade","<= 78.5",">> 78.5"],["Vento","Sim","Nao"]]

classes1 = ["Viajar", "Va", "NaoVa"]

ex1 = [["Sol","25","72","Sim","Va"],
       ["Sol","28","91","Sim","NaoVa"],
       ["Sol","22","70","Nao","Va"],
       ["Sol","23","95","Nao","NaoVa"],
       ["Sol","30","85","Nao","NaoVa"]] 

