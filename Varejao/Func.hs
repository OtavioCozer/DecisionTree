module Func(
 melhorTeste,
 percentagens,
 ehValorNumerico,
 definition1,
 compararCaso,
 refinarBase
)where

import Debug.Trace
import Data.Char
import Data.List    

--calcula length como float--
tamanho :: (Num c, Foldable t) => t a -> c
tamanho x = (fromIntegral . length) x

-- Entropia e entropia' calcula a entropia da base de exemplos--
entropia [] _ = 0
entropia ex classes = entropia' (percentagens ex classes)

entropia' [] = 0
entropia' (ocorrencia:ocorrencias) = - (ocorrencia * (lg ocorrencia)) + entropia' ocorrencias

-----------------------------------------------------------------

--As funções à seguir são usadas parar calcular a percentagens de ocorrência--


percentagens ex (classe:classes) = [    (frequency x ex)/(tamanho ex)     | x <- classes ] 

--frequency classe ([]:_) = 0
frequency classe [] = 0
frequency classe (ex:exs) = if (last ex) == classe then 1 + frequency classe exs else 0 + frequency classe exs --Essa classe está errada???? tenho que avaliar melhor depoisss
                                                                                                               -- em uma das chamas (ex:exs) = [[][][][][]] isso deveria aoontecer??

-------------------------------------------------------------------------------------------------------------------

-- a funcção a seguir implementa a seguinte definição {x E Ex/ values(x,a)=v} mostrada no enunciado do trabalho--
-- ela deve passar a sua base de dados e um valor de uma dada caracteristica
--definition1 ([]:_) _ = []
definition1 ex value | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = (definitionNumerico ex (words value))--Essa classe está errada???? tenho que avaliar melhor depoisss -- em uma das chamas (ex:exs) = [[][][][][]] isso deveria aoontecer??
                     | otherwise = definitionString ex value
                        where cabeca = head (words value)

definitionNumerico ex parseValue | head parseValue == "<=" = [ x | x <- ex, ehMenorString (head x) (parseValue!!1)  ] 
                                 | head parseValue == "<>" = [ x | x <- ex, ehEntreString (head x) (parseValue!!1) (parseValue!!2) ] 
                                 | head parseValue == ">>" = [ x | x <- ex, ehMaiorString (head x) (parseValue!!1)  ] 

definitionString :: [[String]] -> String -> [[String]]
definitionString ex value = [ x | x <- ex, (head x) == value ] 

ehMenorString x y = (read x :: Float) <= (read y :: Float)
ehEntreString x y z = ((read x :: Float) > (read y :: Float)) && ((read x :: Float) <= (read z :: Float))
ehMaiorString x y = (read x :: Float) > (read y :: Float)
---------------------------------------------------------------------------

-- a função abaixo é usada para calcular IG definido no enunciado a primeira caracteristica de ex deve ser a caracteristica da funcao--

definition2 ex caracteristica classes = definition2' ex (tail caracteristica) classes

definition2' ex [] classes = 0
definition2' ex (value:values) classes =  (((tamanho def1) / (tamanho ex)) * (entropia def1 classes)) + (definition2' ex values classes)
                                where def1 = definition1 ex value


ig ex caracteristica classes = (entropia ex classes) - (definition2 ex caracteristica classes)

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--as funçôes abaixo calculam IV com as mesmas condiçoes de IG--
definition3 ex caracteristica classes = definition3' ex (tail caracteristica) classes

definition3' ex [] classes = 0
definition3' ex (value:values) classes = ((modDef1 / modEx) * (lg (modDef1 / modEx)))   + (definition3' ex values classes)
                                where modDef1 = tamanho (definition1 ex value)
                                      modEx = tamanho ex


iv ex caracteristica classes = - (definition3 ex caracteristica classes)

--lg calcula o log na base 2----
lg 0 = 0                                      
lg x = logBase 2 x



--igr conforme definito no enunciado --
igr ex a classes = divisao (ig ex a classes) (iv ex a classes)
 
-- divisao implementa uma divisao por 0--
divisao 0 0 = 0
divisao x y =x/y

--calcula o igr de todas as caracteristicas lembrando que a primeira caracteristica de ex deve ser a--
igrLista _ [] _ = []
igrLista ex (a:as) classes = (igr ex a classes) : (igrLista (map tail ex) as classes)    

--melhor Teste retorna o nome do melhor teste e o seu indice no grupo de caractersiticas--
melhorTeste ex a classes fixedChar = newTuple
                            where igrs = igrLista ex a classes
                                  tuple = retornaMaior igrs a
                                  index = retira (elemIndex (head (fst tuple)) fixedChar)
                                  newTuple = insere tuple index

retira (Just x) = x
insere (x,y) z = (x,y,z) 
--retorna o maior elemento e um indice--
retornaMaior igrs a = retornaMaior' igrs a ((head a),0) 0 (head igrs)

--retornaMaior' a b c d e = (show a) ++ (show b) ++ (show c) ++ (show d) ++ (show e) 
retornaMaior' [] [] melhorTeste _ _ = melhorTeste 
retornaMaior' (igr:igrs) (a:as) maiorElem index maior = if igr > maior then retornaMaior' igrs as (a,index) (index+1) igr else  retornaMaior' igrs as maiorElem (index+1) maior

--verfifica se uma caracteristica é numérica--
ehValorNumerico value = ehValorNumerico' (words (value)) 

ehValorNumerico'  parseValue | head parseValue == "<=" = True
                                      | head parseValue == "<>" = True 
                                      | head parseValue == ">>" = True
                                      | otherwise = False

--A fincao abaixo determina se um dado caso possui um dado valor de uma caracteristica--
compararCaso valueCaso valueCaracteristica | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = compararCasoNumerico  valueCaso (words valueCaracteristica)
                                           | otherwise = valueCaso == valueCaracteristica 
                                             where cabeca = head (words valueCaracteristica)

compararCasoNumerico valueCaso parseValue | head parseValue == "<=" = ehMenorString valueCaso (parseValue!!1)  
                                          | head parseValue == "<>" = ehEntreString valueCaso (parseValue!!1) (parseValue!!2) 
                                          | head parseValue == ">>" = ehMaiorString valueCaso (parseValue!!1)  




----
--refinarBase ([]:_) _ = []
refinarBase ex value index | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = (refinarBaseNumerico ex (words value) index)--Essa classe está errada???? tenho que avaliar melhor depoisss -- em uma das chamas (ex:exs) = [[][][][][]] isso deveria aoontecer??
                           | otherwise = refinarBaseString ex value index
                              where cabeca = head (words value)

refinarBaseNumerico ex parseValue index | head parseValue == "<=" = [ x | x <- ex, ehMenorString (x!!index) (parseValue!!1) ] 
                                        | head parseValue == "<>" = [ x | x <- ex, ehEntreString (x!!index) (parseValue!!1) (parseValue!!2) ] 
                                        | head parseValue == ">>" = [ x | x <- ex, ehMaiorString (x!!index) (parseValue!!1)  ] 


refinarBaseString ex value index = [ x | x <- ex, (x!!index) == value ] 

ex1 = [["Nublado","20","20","Sim","Va"],
      ["Sol","22","20","Nao","NaoVa"],
      ["Nublado","22","20","Sim","Va"],
      ["Sol","22","20","Nao","NaoVa"],
      ["Nublado","23","20","Nao","NaoVa"],
      ["Chuva","22","25","Sim","Va"],
      ["Nublado","23","25","Nao","NaoVa"],
      ["Chuva","23","20","Sim","Va"],
      ["Nublado","23","20","Nao","NaoVa"],
      ["Sol","23","20","Nao","NaoVa"]]

c1 = [["Aparencia","Sol","Chuva","Nublado"],["Temperatura","<= 22.5","<> 22.5 30.0","<> 30.0 30.5",">> 30.5"],["Umidade","<= 60.0",">> 60.0"],["Vento","Sim","Nao"]]
class1 = ["Viajar","Va","NaoVa"]