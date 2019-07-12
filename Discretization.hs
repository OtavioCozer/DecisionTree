module Discretization(
 insereValoresNumericos
)where

import Data.List

-- as funçoes abaixo determinam quai caractersiticas são numéricas--
indexColunasNumericas :: Num a => [[String]] -> [a]
indexColunasNumericas caracteristicas = indexColunasNumericas' 0 caracteristicas

indexColunasNumericas' :: Num a => a -> [[String]] -> [a]
indexColunasNumericas' _ [] = []
indexColunasNumericas' count (caracteristica:caracteristicas) = if ehColunaNumerica caracteristica then count : indexColunasNumericas' (count+1) caracteristicas else indexColunasNumericas' (count+1) caracteristicas 

ehColunaNumerica :: [String] -> Bool
ehColunaNumerica (a:[]) = True
ehColunaNumerica (a:as) = (head (words (head as))) == "<=" || (head (words (head as))) == "<>" || (head (words (head as)) == ">>") 
ehColunaNumerica _ = False  

-- passo1 e cria colunas e juntaCOlunas são reponsável por receber uma base de dados e as caractersiticas e retornar n listas de tuplas para cada elemento da base sendo q n é a quantiddade de caracterisitcas numéricas -- 
-- e deppois juntalas em listas de tuplas de uma mamesma caracteristica--
passo1 :: [[t]] -> [[String]] -> [[(t, t)]]
passo1 ex caracteristicas =  passo1' ex (indexColunasNumericas caracteristicas)

passo1' :: [[t]] -> [Int] -> [[(t, t)]]
passo1' [] _ = []
passo1' (ex:exs) index = criaColunas ex index (last ex) : passo1' exs index

criaColunas :: [a] -> [Int] -> t -> [(a, t)]
criaColunas _ [] _ = []
criaColunas ex (index:indexs) classe = (ex!!index, classe) : criaColunas ex indexs classe 

juntaColunas :: [[t]] -> [[String]] -> [[(t, t)]]
juntaColunas ex caracteristicas = juntaColunas' (passo1 ex caracteristicas)

juntaColunas' :: [[a]] -> [[a]]
juntaColunas'  ([]:_) = []
juntaColunas' tuplas = map head tuplas : juntaColunas' (map tail tuplas)

--ordena colunas é reponsável por ordenar cada coluna de caracteristica--
ordenaColunas :: [[(String, b)]] -> [[(String, b)]]
ordenaColunas [] = []
ordenaColunas (coluna:colunas) = (sortBy compara coluna) : ordenaColunas colunas

--compara é uma função implementada para ser usada no sort genéric0
compara :: (String, b1) -> (String, b2) -> Ordering
compara a b | ehMaiorString (fst a) (fst b) = GT
            | ehMenorString (fst a) (fst b) = LT
            | otherwise = EQ

--Foi necessário as funçoes para fazer comparacoes entre strings
ehMenorString :: String -> String -> Bool
ehMenorString x y = (read x :: Float) <=(read y :: Float)

ehMaiorString :: String -> String -> Bool
ehMaiorString x y = (read x :: Float) > (read y :: Float)

--usadao para calcular as medianas que gerarao os intervalos
determinaMedianas :: Eq b => [(String, b)] -> [String]
determinaMedianas (_:[]) = []
determinaMedianas (c:cs)  = if snd c == snd (head cs) then determinaMedianas cs else mediana (fst c) (fst (head cs))  : determinaMedianas cs

mediana :: String -> String -> String
mediana x y = show (((read x :: Double) + (read y :: Double)) / 2.0)

--determina quais sao os vlaores numericos para cada caracteristicas
determinaValoresNumericos :: [[String]] -> [[String]] -> [[String]]
determinaValoresNumericos ex caracteristicas = map determinaMedianas(ordenaColunas (juntaColunas ex caracteristicas)) 

--cria os reais valores que serãp usadaos nas comparações
criaIntervalos :: [[Char]] -> [[Char]]
criaIntervalos xs = criaIntervalos' xs True (head xs)

criaIntervalos' :: [[Char]] -> Bool -> [Char] -> [[Char]]
criaIntervalos' (x:xs) ehInicio inferior | ehInicio && (ehFinal (x:xs)) = ("<= " ++ x) : (">> " ++ x) : []
                                         | ehInicio == True = ("<= " ++ x) : criaIntervalos' xs False x
                                         | ehFinal (x:xs) = ("<> "++ inferior ++ " " ++ x) : [">> " ++ x]
                                         | otherwise = ("<> "++ inferior ++ " " ++ x)  : criaIntervalos' xs False x

--insere os valores nas caracterisricas
insereValoresNumericos :: [[String]] -> [[String]] -> [[String]]       
insereValoresNumericos ex caracteristicas = insereValoresNumericos' caracteristicas (map criaIntervalos(determinaValoresNumericos ex caracteristicas))

insereValoresNumericos' :: [[String]] -> [[String]] -> [[String]]
insereValoresNumericos' [] [] = []
insereValoresNumericos' (caracteristica:caracteristicas) [] = (caracteristica:caracteristicas)
insereValoresNumericos' (caracteristica:caracteristicas) (intervalo:intervalos) = if (ehColunaNumerica caracteristica) then ([head caracteristica] ++ intervalo) : insereValoresNumericos' caracteristicas intervalos else caracteristica : insereValoresNumericos' caracteristicas (intervalo:intervalos)
                  
ehFinal :: [a] -> Bool
ehFinal (_:[]) = True
ehFinal _ = False
         
