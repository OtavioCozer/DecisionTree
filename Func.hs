module Func(
 melhorTeste,
 percentagens,
 ehValorNumerico,
 definition1
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

frequency classe [] = 0
frequency classe (ex:exs) = if (last ex) == classe then 1 + frequency classe exs else 0 + frequency classe exs 


-------------------------------------------------------------------------------------------------------------------

-- a funcção a seguir implementa a seguinte definição {x E Ex/ values(x,a)=v} mostrada no enunciado do trabalho--
-- ela deve passar a sua base de dados e um valor de uma dada caracteristica

definition1 ex value | (cabeca == "<=" || cabeca == "<>" || cabeca == ">>")  = definitionNumerico ex (words value)
                     | otherwise = definitionString ex value
                        where cabeca = head (words value)

definitionNumerico ex parseValue | head parseValue == "<=" = [ x | x <- ex, (head x) <= (parseValue!!1)  ] 
                                 | head parseValue == "<>" = [ x | x <- ex, ((head x) > (parseValue!!1)) && ((head x) <= (parseValue!!2)) ] 
                                 | head parseValue == ">>" = [ x | x <- ex, (head x) > (parseValue!!1)  ] 

definitionString :: [[String]] -> String -> [[String]]
definitionString ex value = [ x | x <- ex, (head x) == value ] 

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
melhorTeste ex a classes = retornaMaior igrs a
                            where igrs = igrLista ex a classes

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

caracteristicas1 = [["Aparencia","Sol","Chuva","Nublado"],["Temperatura","<= 22.5","<> 22.5 24.0","<> 24.0 26.5",">> 26.5"],["Umidade","<= 78.5",">> 78.5"],["Vento","Sim","Nao"]]

char = ["Temperatura","<= 22.5","<> 22.5 24.0","<> 24.0 26.5",">> 26.5"]

classes1 = ["Viajar", "Va", "NaoVa"]

ex1 = [["Sol","25","72","Sim","Va"],
       ["Sol","28","91","Sim","NaoVa"],
       ["Sol","22","70","Nao","Va"],
       ["Sol","23","95","Nao","NaoVa"],
       ["Sol","30","85","Nao","NaoVa"]]

ex2 = [["25","72","Sim","Va"],
       ["28","91","Sim","NaoVa"],
       ["22","70","Nao","Va"],
       ["23","95","Nao","NaoVa"],
       ["30","85","Nao","NaoVa"]]      
       
ex3 = [["25","72","Sim","Va"],["28","91","Sim","NaoVa"],["22","70","Nao","Va"],["23","95","Nao","NaoVa"],["30","85","Nao","NaoVa"]]
value = "<> 22.5 24.0"

values = ["1<= 22.5","<> 22.5 24.0","<> 24.0 26.5",">> 26.5"]