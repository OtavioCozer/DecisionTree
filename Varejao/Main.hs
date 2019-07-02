import Leitura
import Func
import Discretization
import Debug.Trace

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String, index :: Int} 
          | Folha{classe :: String, resposta :: String} 
          | Nil deriving Show

--Para gerar o profile:
       --Compilar: ghc -prof -fprof-auto -rtsopts --make .\Main.hs
       --Run: .\Main.exe +RTS -p

--Para medir o tempo: 
       --Run: Measure-Command {start-process .\Main.exe -Wait}

--Para debugar:
       --Compilar:
main = do
            baseIO <- readFile "base.txt"
            caracteristicasIO <- readFile "descricao.txt"
            casoIO <- readFile "caso.txt"
           
            let base = leEntrada baseIO
            let caracteristicas' = leEntrada caracteristicasIO
            let caracteristicas = insereValoresNumericos base (init caracteristicas') 
            let classes = last caracteristicas'
            let casos = leEntrada casoIO
            let fixedChar = map head caracteristicas
            let arvore = arvoreDecisao base caracteristicas (maioria base classes []) classes "" fixedChar
            let classified = classificarTodos casos arvore
           
            putStrLn $ show base
            --putStrLn $ show ( init caracteristicas')           
            putStrLn $ show caracteristicas
            putStrLn $ show (maioria base classes [])
            putStrLn $ show classes 
            --putStrLn $ show casos
            putStrLn ("\n")
            writeFile "arv.txt" (show arvore)            --putStrLn (show classified)
            writeFile "result.txt" (show classified)
            putStrLn ("OKKK3")


--implementa o algoritmo da árvore de decisão--


arvoreDecisao exemplos caracteristicas maisComum classes value fixedChar | exemplos == [] = Folha{classe = maisComum, resposta = value} 
                                                                         | mesmaClasse exemplos = Folha{classe = (last (head exemplos)), resposta = value}
                                                                         | caracteristicas == [] = Folha{classe = maioria exemplos classes [], resposta = value} 
                                                                         | otherwise = criaNo exemplos caracteristicas maisComum classes value fixedChar

criaNo exemplos caracteristicas maisComum classes value fixedChar = (forEachValue arvore exemplos newCaracteristicas values maisComum classes newExemplos melhorIndex fixedChar)
                                                               where melhor =trace("--EXEMPLOs=" ++ (show exemplos) ++ "--\n--CARACTERISTICAS=" ++ (show caracteristicas) ++ "--\n--CLASSES=" ++ (show classes) ++ "--\n--FIXEDCHAR=" ++ (show fixedChar) ++ "--\n" ) (melhorTeste exemplos caracteristicas classes fixedChar)
                                                                     melhorChar = trace("--MELHOR="++ show melhor ++ "--") (fst' melhor)
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
                                                                             where exemplosi = trace ("\n--VALOR UTILIZADO= " ++ (show value) ++ "--\n") (refinarBase exemplos value index) --toda hora chamar map drop index é burro. tem q ajeita isso 
                                                                                   subArvore =trace("\n--EXEMPLOS ATUALIZADOS= " ++ (show exemplosAtualizados) ++ "--\n--CARACTERISTICAS ATUALIZADAS =" ++ (show caracteristicas) ++ "--\n")(arvoreDecisao exemplosAtualizados caracteristicas (maioria exemplosAtualizados classes maisComum) classes value fixedChar) 
                                                                                   newArvore = adicionaFilho arvore subArvore
                                                                                   exemplosAtualizados =trace ("\n--EXEMPLOS FILTRADOS= " ++ (show exemplosi)++"--\n") (map (removeItem (index)) exemplosi)
{- refinandoBase exemplos value | ehValorNumerico value = refinandoBaseNumerico exemplos value
                             | otherwise = refinandoBaseNumerico exemplos value -}

adicionaFilho (No a filhos b i) subArvore = (No a (subArvore : filhos) b i)

maioria [] classes maisComum = maisComum
maioria ex classes maisComum = maioria' ocorrencias (tail classes) (maximum ocorrencias)
                                   where ocorrencias = percentagens ex classes 

maioria' (o:os) (c:cs) maior = if maior == o then c else maioria' os cs maior

------------
classificarTodos cs arv = [ classificar x arv | x <- cs ]

classificar c (Folha classe' _) = classe'
classificar c (No b (a:as) d i) = if compararCaso (c!!i) value then classificar c a  else classificar c (No b as d i) 
                                          where value = resposta a
                             

removeItem 0 (y:ys) = ys
removeItem n (y:ys) = y : removeItem (n-1) ys


trd (_,_,x) = x
fst' (x,_,_) = x
snd' (_,x,_) = x

teste1 = [["Chuva","30","72","Sim","Va"],["Nublado","28","91","Sim","NaoVa"],["Sol","22","100","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Nublado","30","85","Nao","NaoVa"],["Nublado","22","72","Sim","Va"],["Sol","28","70","Sim","NaoVa"],["Chuva","22","70","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Nublado","30","85","Nao","NaoVa"]]
value1 = "<= 22.5"