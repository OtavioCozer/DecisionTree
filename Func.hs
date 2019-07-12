module Func(
 melhorTeste,
 percentagens,
 ehValorNumerico,
 definition1,
 compararCaso,
 refinarBase
)where

import Data.Char
import Data.List    

--calcula length como float--
tamanho :: (Num c, Foldable t) => t a -> c
tamanho x = (fromIntegral . length) x

-- Entropia e entropia' calcula a entropia da base de exemplos--
entropia :: (Floating p, Eq p, Eq t) => [[t]] -> [t] -> p
entropia [] _ = 0
entropia ex classes = entropia' (percentagens ex classes)

entropia' :: (Eq p, Floating p) => [p] -> p
entropia' [] = 0
entropia' (ocorrencia:ocorrencias) = - (ocorrencia * (lg ocorrencia)) + entropia' ocorrencias


--As funções à seguir são usadas parar calcular a percentagens de ocorrência--
percentagens :: (Fractional a, Eq t) => [[t]] -> [t] -> [a]
percentagens ex (classe:classes) = [    (frequency x ex)/(tamanho ex)     | x <- classes ] 

--calcula a frequencia de ocorrencia de uma classe na lista
frequency :: (Num a, Eq t) => t -> [[t]] -> a
frequency classe [] = 0
frequency classe (ex:exs) = if (last ex) == classe then 1 + frequency classe exs else 0 + frequency classe exs 


-- a funcção a seguir implementa a seguinte definição {x E Ex/ values(x,a)=v} mostrada no enunciado do trabalho--
-- ela deve passar a sua base de dados e um valor de uma dada caracteristica
-- é definido em duas funcoes para caracteristicas numericas e strings
definition1 :: [[String]] -> String -> [[String]]
definition1 ex value | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = (definitionNumerico ex (words value))--Essa classe está errada???? tenho que avaliar melhor depoisss -- em uma das chamas (ex:exs) = [[][][][][]] isso deveria aoontecer??
                     | otherwise = definitionString ex value
                        where cabeca = head (words value)

definitionNumerico :: [[String]] -> [[Char]] -> [[String]]
definitionNumerico ex parseValue | head parseValue == "<=" = [ x | x <- ex, ehMenorString (head x) (parseValue!!1)  ] 
                                 | head parseValue == "<>" = [ x | x <- ex, ehEntreString (head x) (parseValue!!1) (parseValue!!2) ] 
                                 | head parseValue == ">>" = [ x | x <- ex, ehMaiorString (head x) (parseValue!!1)  ] 

definitionString :: [[String]] -> String -> [[String]]
definitionString ex value = [ x | x <- ex, (head x) == value ] 

--funcose usadas para comparar strings ocmo se fossem numeros
ehMenorString :: String -> String -> Bool
ehMenorString x y = (read x :: Float) <= (read y :: Float)

ehEntreString :: String -> String -> String -> Bool
ehEntreString x y z = ((read x :: Float) > (read y :: Float)) && ((read x :: Float) <= (read z :: Float))

ehMaiorString :: String -> String -> Bool
ehMaiorString x y = (read x :: Float) > (read y :: Float)

-- a função abaixo é usada para calcular IG definido no enunciado a primeira caracteristica de ex deve ser a caracteristica da funcao--

definition2 :: (Floating p, Eq p) => [[String]] -> [String] -> [String] -> p
definition2 ex caracteristica classes = definition2' ex (tail caracteristica) classes

definition2' :: (Floating p, Eq p) => [[String]] -> [String] -> [String] -> p
definition2' ex [] classes = 0
definition2' ex (value:values) classes =  (((tamanho def1) / (tamanho ex)) * (entropia def1 classes)) + (definition2' ex values classes)
                                where def1 = definition1 ex value

ig :: (Floating a, Eq a) => [[String]] -> [String] -> [String] -> a
ig ex caracteristica classes = (entropia ex classes) - (definition2 ex caracteristica classes)

--as funçôes abaixo calculam IV com as mesmas condiçoes de IG--
definition3 :: (Eq p, Floating p) => [[String]] -> [String] -> t -> p
definition3 ex caracteristica classes = definition3' ex (tail caracteristica) classes

definition3' :: (Eq p, Floating p) => [[String]] -> [String] -> t -> p
definition3' ex [] classes = 0
definition3' ex (value:values) classes = ((modDef1 / modEx) * (lg (modDef1 / modEx)))   + (definition3' ex values classes)
                                where modDef1 = tamanho (definition1 ex value)
                                      modEx = tamanho ex

iv :: (Eq a, Floating a) => [[String]] -> [String] -> t -> a
iv ex caracteristica classes = - (definition3 ex caracteristica classes)

--lg calcula o log na base 2----
lg :: (Eq p, Floating p) => p -> p
lg 0 = 0                                      
lg x = logBase 2 x

--igr conforme definito no enunciado --
igr :: (Eq p, Floating p) => [[String]] -> [String] -> [String] -> p
igr ex a classes = divisao (ig ex a classes) (iv ex a classes)
 
-- divisao implementa uma divisao por 0--
divisao :: (Eq p, Fractional p) => p -> p -> p
divisao 0 0 = 0
divisao x y =x/y

--calcula o igr de todas as caracteristicas lembrando que a primeira caracteristica de ex deve ser a--
igrLista :: (Eq a, Floating a) => [[String]] -> [[String]] -> [String] -> [a]
igrLista _ [] _ = []
igrLista ex (a:as) classes = (igr ex a classes) : (igrLista (map tail ex) as classes)    

--melhor Teste retorna o nome do melhor teste e o seu indice no grupo de caractersiticas--
melhorTeste
  :: Num a => [[String]] -> [[String]] -> [String] -> [String] -> ([String], a, Int)
melhorTeste ex a classes fixedChar = newTuple
                            where igrs = igrLista ex a classes
                                  tuple = retornaMaior igrs a
                                  index = retira (elemIndex (head (fst tuple)) fixedChar)
                                  newTuple = insere tuple index

--retira é usado para desimcapsular um Maybe
retira :: Maybe a -> a                                  
retira (Just x) = x

--usado para criar uma tripla 
insere :: (a, b) -> c -> (a, b, c)
insere (x,y) z = (x,y,z) 

--retorna o maior elemento e um indice--
retornaMaior :: (Ord a1, Num a2) => [a1] -> [a3] -> (a3, a2)
retornaMaior igrs a = retornaMaior' igrs a ((head a),0) 0 (head igrs)

retornaMaior' :: (Ord t, Num a1) => [t] -> [a2] -> (a2, a1) -> a1 -> t -> (a2, a1)
retornaMaior' [] [] melhorTeste _ _ = melhorTeste 
retornaMaior' (igr:igrs) (a:as) maiorElem index maior = if igr > maior then retornaMaior' igrs as (a,index) (index+1) igr else  retornaMaior' igrs as maiorElem (index+1) maior

--verfifica se uma caracteristica é numérica--
ehValorNumerico :: String -> Bool
ehValorNumerico value = ehValorNumerico' (words (value)) 

ehValorNumerico' :: [[Char]] -> Bool
ehValorNumerico'  parseValue | head parseValue == "<=" = True
                                      | head parseValue == "<>" = True 
                                      | head parseValue == ">>" = True
                                      | otherwise = False

--A fincao abaixo determina se um dado caso possui um dado valor de uma caracteristica--
compararCaso :: String -> String -> Bool
compararCaso valueCaso valueCaracteristica | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = compararCasoNumerico  valueCaso (words valueCaracteristica)
                                           | otherwise = valueCaso == valueCaracteristica 
                                             where cabeca = head (words valueCaracteristica)

compararCasoNumerico :: String -> [[Char]] -> Bool
compararCasoNumerico valueCaso parseValue | head parseValue == "<=" = ehMenorString valueCaso (parseValue!!1)  
                                          | head parseValue == "<>" = ehEntreString valueCaso (parseValue!!1) (parseValue!!2) 
                                          | head parseValue == ">>" = ehMaiorString valueCaso (parseValue!!1)  

--usado para separar a base de acorodo com a caracteristica escolhihda
refinarBase :: [[String]] -> String -> Int -> [[String]]
refinarBase ex value index | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = (refinarBaseNumerico ex (words value) index)
                           | otherwise = refinarBaseString ex value index
                              where cabeca = head (words value)

refinarBaseNumerico :: [[String]] -> [[Char]] -> Int -> [[String]]
refinarBaseNumerico ex parseValue index | head parseValue == "<=" = [ x | x <- ex, ehMenorString (x!!index) (parseValue!!1) ] 
                                        | head parseValue == "<>" = [ x | x <- ex, ehEntreString (x!!index) (parseValue!!1) (parseValue!!2) ] 
                                        | head parseValue == ">>" = [ x | x <- ex, ehMaiorString (x!!index) (parseValue!!1)  ] 

refinarBaseString :: Eq a => [[a]] -> a -> Int -> [[a]]
refinarBaseString ex value index = [ x | x <- ex, (x!!index) == value ] 