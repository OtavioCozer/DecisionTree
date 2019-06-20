module Func(
 igrLista
)where

import Debug.Trace

import Data.List    

--calcula length como float--
tamanho :: (Num c, Foldable t) => t a -> c
tamanho x = (fromIntegral . length) x

-- Entropia e entropia' calcula a entropia da base de exemplos--
entropia [] _ = 0
entropia ex classes = entropia' (percentagens ex classes)

entropia' [] = 0
entropia' (ocorrencia:ocorrencias) = - (ocorrencia * (logBase 2 ocorrencia)) + entropia' ocorrencias

-----------------------------------------------------------------

--As funções à seguir são usadas parar calcular a percentagens de ocorrência--


percentagens ex (classe:classes) = [    (frequency x ex)/(tamanho ex)     | x <- classes ] 

frequency classe [] = 0
frequency classe (ex:exs) = if (last ex) == classe then 1 + frequency classe exs else 0 + frequency classe exs 


-------------------------------------------------------------------------------------------------------------------

-- a funcção a seguir implementa a seguinte definição {x E Ex/ values(x,a)=v} mostrada no enunciado do trabalho--
-- ela deve passar a sua base de dados e um valor de uma dada caracteristica

definition1 :: [[String]] -> String -> [[String]]
definition1 ex value = [ x | x <- ex, (head x) == value ] 

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
lg 0 = 1                                      
lg x = logBase 2 x



--igr conforme definito no enunciado --
igr ex a classes = (ig ex a classes) / (iv ex a classes)
 
--calcula o igr de todas as caracteristicas lembrando que a primeira caracteristica de ex deve ser a--
igrLista _ [] _ = []
igrLista ex (a:as) classes = (igr ex a classes) : trace ("\n\ncalling igr lista with ex = "++ show (map tail ex) ++ " (a:as) = " ++ show as ++ " classes = " ++ show classes) (igrLista (map tail ex) as classes)    



caracteristicas1 = [["Aparencia" , "Sol" , "Chuva" , "Nublado"] , ["Vento", "Sim", "Nao"]]

classes1 = ["Viajar", "Va", "NaoVa"]

ex1 = [["Sol","Sim","Va"],
       ["Sol","Sim","NaoVa"],
       ["Sol","Nao","Va"],
       ["Sol","Nao","NaoVa"],
       ["Sol","Nao","NaoVa"]] 