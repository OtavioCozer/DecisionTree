import Leitura
import Func
import Discretization
import Debug.Trace

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String, index :: Int} 
          | Folha{classe :: String, resposta :: String} 
          | Nil deriving Show


main = do
            baseIO <- readFile "base1.txt"
            caracteristicasIO <- readFile "descricao1.txt"
            casoIO <- readFile "caso1.txt"
            let base = leEntrada baseIO
            let caracteristicas' = leEntrada caracteristicasIO
            let caracteristicas = insereValoresNumericos base (init caracteristicas') 
            let classes = last caracteristicas'
            let casos = leEntrada casoIO
            let fixedChar = map head caracteristicas
            let arvore = arvoreDecisao base caracteristicas (maioria base classes) classes "" fixedChar
            let classified = classificarTodos casos arvore
            putStrLn $ show base
            --putStrLn $ show ( init caracteristicas')           
            --putStrLn $ show caracteristicas
            --putStrLn $ show (maioria base classes)
            --putStrLn $ show classes 
            --putStrLn $ show casos
            writeFile "arv.txt" (show arvore)            --putStrLn (show classified)
            writeFile "result.txt" (show classified)
            putStrLn ("OKKK")


--implementa o algoritmo da árvore de decisão--


arvoreDecisao exemplos caracteristicas maisComum classes value fixedChar | exemplos == [] = Folha{classe = maisComum, resposta = value} 
                                                                         | mesmaClasse exemplos = Folha{classe = (last (head exemplos)), resposta = value}
                                                                         | caracteristicas == [] = Folha{classe = maioria exemplos classes, resposta = value} 
                                                                         | otherwise = criaNo exemplos caracteristicas maisComum classes value fixedChar

criaNo exemplos caracteristicas maisComum classes value fixedChar = (forEachValue arvore exemplos newCaracteristicas values maisComum classes newExemplos melhorIndex fixedChar)
                                                               where melhor = melhorTeste exemplos caracteristicas classes fixedChar
                                                                     melhorChar = fst' melhor
                                                                     melhorIndex = snd' melhor
                                                                     arvIndex = trd melhor
                                                                     values = tail (melhorChar)
                                                                     arvore = raiz (head melhorChar) value arvIndex
                                                                     newCaracteristicas = filter (/=melhorChar) caracteristicas
                                                                     newExemplos = map (drop melhorIndex) exemplos

-- As funções mesmaClasse e mesmaClasse' são usadas para verificar se um conjunto de dados possui todas as classes iguais
mesmaClasse :: [[String]] -> Bool
mesmaClasse (x:xs) =  mesmaClasse' (last x) (x:xs) True             

mesmaClasse' :: String -> [[String]] -> Bool -> Bool
mesmaClasse' _ [] True = True
mesmaClasse' anterior (x:xs) igual  = if classe == anterior then mesmaClasse' classe xs True else False
                                    where classe =  last x


raiz melhor value indice = No {pergunta = melhor, filhos = [], resposta = value, index=indice}

forEachValue arvore _ _ [] _ _ _ _ _ = arvore 
forEachValue arvore exemplos caracteristicas (value:values) maisComum classes newExemplos index fixedChar = forEachValue newArvore exemplos caracteristicas values maisComum classes newExemplos index fixedChar
                                                                             where exemplosi = refinarBase exemplos value index --toda hora chamar map drop index é burro. tem q ajeita isso 
                                                                                   subArvore = {- trace("\n--Calling exemplosi=" ++ (show exemplosi) ++ " And exemplosatualizados=" ++ (show exemplosAtualizados) ++ " And caracteristicas=" ++ (show caracteristicas) ) -}(arvoreDecisao exemplosAtualizados caracteristicas (maioria exemplos classes) classes value fixedChar) 
                                                                                   newArvore = adicionaFilho arvore subArvore
                                                                                   exemplosAtualizados = map (removeItem index) exemplosi
{- refinandoBase exemplos value | ehValorNumerico value = refinandoBaseNumerico exemplos value
                             | otherwise = refinandoBaseNumerico exemplos value -}

adicionaFilho (No a filhos b i) subArvore = (No a (subArvore : filhos) b i)

melhorTeste1 ex a classes = (["Umidade","<= 78.5",">> 78.5"],2)

--refinandoBaseNumerico exemplos values = [[["Sol","25","72","Sim","Va"],["Sol","28","91","Sim","NaoVa"],["Sol","22","70","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Sol","30","85","Nao","NaoVa"]]] 

maioria ex classes = maioria' ocorrencias (tail classes) (maximum ocorrencias)
                                   where ocorrencias = percentagens ex classes 

maioria' (o:os) (c:cs) maior = if maior == o then c else maioria' os cs maior

------------
classificarTodos cs arv = [ classificar x arv | x <- cs ]

classificar c (Folha classe' _) = classe'
classificar c (No b (a:as) d i) = if compararCaso (c!!i) value then classificar c a  else classificar c (No b as d i) 
                                          where value = resposta a
                             

removeItem 0 (y:ys) = ys
removeItem n (y:ys) = y : removeItem (n-1) ys


arv1 = No {pergunta = "Umidade", filhos = [Folha {classe = "NaoVa", resposta = ">> 78.5"},Folha {classe = "Va", resposta = "<= 78.5"}], resposta = "", index = 2}

casos1 = ["Chuva","23","92","Sim"]

moreCommom= "NaoVa"

teste = ">> 78.5"

trd (_,_,x) = x
fst' (x,_,_) = x
snd' (_,x,_) = x

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


test1 = No {pergunta = "Umidade", filhos = [], resposta = "", index=1}

test2 = No {pergunta = "oiee", filhos = [], resposta = "", index=2}