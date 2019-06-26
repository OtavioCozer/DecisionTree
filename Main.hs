import Leitura
import Func
import Discretization

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String} 
          | Folha{classe :: String, resposta :: String} 
          | Nil deriving Show


main = do
            nome <- readFile "base.txt"
            putStrLn  $ show (leEntrada nome)


--implementa o algoritmo da árvore de decisão--


arvoreDecisao exemplos caracteristicas maisComum classes value | exemplos == [] = Folha{classe = maisComum, resposta = value} 
                                                               | mesmaClasse exemplos = Folha{classe = (last (head exemplos)), resposta = value}
                                                               | caracteristicas == [] = Folha{classe = maioria exemplos classes, resposta = value} 
                                                               | otherwise = criaNo exemplos caracteristicas maisComum classes value

criaNo exemplos caracteristicas maisComum classes value = forEachValue arvore exemplos newCaracteristicas values maisComum classes newExemplos
                                                        where melhor = melhorTeste exemplos caracteristicas classes
                                                              melhorChar = fst melhor
                                                              melhorIndex = snd melhor
                                                              values = tail (melhorChar)
                                                              arvore = raiz (head melhorChar) value
                                                              newCaracteristicas = filter (/=melhorChar) caracteristicas
                                                              newExemplos = map (drop melhorIndex) exemplos

-- As funções mesmaClasse e mesmaClasse' são usadas para verificar se um conjunto de dados possui todas as classes iguais
mesmaClasse :: [[String]] -> Bool
mesmaClasse (x:xs) = mesmaClasse' (last x) (x:xs) True             

mesmaClasse' :: String -> [[String]] -> Bool -> Bool
mesmaClasse' _ [] True = True
mesmaClasse' anterior (x:xs) igual  = if classe == anterior then mesmaClasse' classe xs True else False
                                    where classe =  last x


raiz melhor value = No {pergunta = melhor, filhos = [], resposta = value}

forEachValue arvore _ _ [] _ _ _ = arvore 
forEachValue arvore exemplos caracteristicas (value:values) maisComum classes newExemplos = forEachValue newArvore exemplos caracteristicas values maisComum classes newExemplos
                                                                             where exemplosi = definition1 newExemplos value --toda hora chamar map drop index é burro. tem q ajeita isso 
                                                                                   subArvore = arvoreDecisao exemplosi caracteristicas (maioria exemplos classes) classes value 
                                                                                   newArvore = adicionaFilho arvore subArvore
{- refinandoBase exemplos value | ehValorNumerico value = refinandoBaseNumerico exemplos value
                             | otherwise = refinandoBaseNumerico exemplos value -}

adicionaFilho (No a filhos b) subArvore = (No a (subArvore : filhos) b)

melhorTeste1 ex a classes = (["Umidade","<= 78.5",">> 78.5"],2)

--refinandoBaseNumerico exemplos values = [[["Sol","25","72","Sim","Va"],["Sol","28","91","Sim","NaoVa"],["Sol","22","70","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Sol","30","85","Nao","NaoVa"]]] 

maioria ex classes = maioria' ocorrencias (tail classes) (maximum ocorrencias)
                                   where ocorrencias = percentagens ex classes 

maioria' (o:os) (c:cs) maior = if maior == o then c else maioria' os cs maior


moreCommom= "NaoVa"

teste = ">> 78.5"



caracteristicas1 = [["Aparencia","Sol","Chuva","Nublado"],["Temperatura","<= 22.5","<> 22.5 24.0","<> 24.0 26.5",">> 26.5"],["Umidade","<= 78.5",">> 78.5"],["Vento","Sim","Nao"]]

classes1 = ["Viajar", "Va", "NaoVa"]
                                    
ex1 = [["Sol","25","72","Sim","Va"],
       ["Sol","28","91","Sim","NaoVa"],
       ["Sol","22","70","Nao","Va"],
       ["Sol","23","95","Nao","NaoVa"],
       ["Sol","30","85","Nao","NaoVa"]] 
                                    
ex2 = [["Sol","28","91","Sim","NaoVa"],
       ["Sol","23","95","Nao","NaoVa"],
       ["Sol","30","85","Nao","NaoVa"]] 


test1 = No {pergunta = "Umidade", filhos = [], resposta = ""}

test2 = No {pergunta = "oiee", filhos = [], resposta = ""}