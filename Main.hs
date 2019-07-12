import Leitura
import Func
import Discretization
import Text.Printf

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String, index :: Int} 
          | Folha{classe :: String, resposta :: String} 
          | Nil deriving Show


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
           

            writeFile "arvore.txt" ( tail (imprimirArv arvore 0))
            writeFile "result.txt" (unlines classified)



--as funcoes abaixo implementam o algritmo de arvore de decisao descrita no enunciado
arvoreDecisao :: [[String]] -> [[String]] -> String -> [String] -> String -> [String] -> Arv
arvoreDecisao exemplos caracteristicas maisComum classes value fixedChar | exemplos == [] = Folha{classe = maisComum, resposta = value} 
                                                                         | mesmaClasse exemplos = Folha{classe = (last (head exemplos)), resposta = value}
                                                                         | caracteristicas == [] = Folha{classe = maioria exemplos classes [], resposta = value} 
                                                                         | otherwise = criaNo exemplos caracteristicas maisComum classes value fixedChar

criaNo :: [[String]] -> [[String]] -> String -> [String] -> String -> [String] -> Arv
criaNo exemplos caracteristicas maisComum classes value fixedChar = (forEachValue arvore exemplos newCaracteristicas values maisComum classes newExemplos melhorIndex fixedChar)
                                                               where melhor = (melhorTeste exemplos caracteristicas classes fixedChar)
                                                                     melhorChar = (fst' melhor)
                                                                     melhorIndex = snd' melhor
                                                                     arvIndex = trd melhor
                                                                     values = tail (melhorChar)
                                                                     arvore = raiz (head melhorChar) value arvIndex
                                                                     newCaracteristicas = filter (/=melhorChar) caracteristicas
                                                                     newExemplos = map (drop melhorIndex) exemplos


forEachValue :: Arv -> [[String]] -> [[String]] -> [String] -> String -> [String] -> [[String]] -> Int -> [String] -> Arv
forEachValue arvore _ _ [] _ _ _ _ _ = arvore 
forEachValue arvore exemplos caracteristicas (value:values) maisComum classes newExemplos index fixedChar = forEachValue newArvore exemplos caracteristicas values maisComum classes newExemplos index fixedChar
                                                                             where exemplosi = (refinarBase exemplos value index) --toda hora chamar map drop index é burro. tem q ajeita isso 
                                                                                   subArvore = (arvoreDecisao exemplosAtualizados (insereValoresNumericos exemplosAtualizados caracteristicas) (maioria exemplosAtualizados classes maisComum) classes value fixedChar) 
                                                                                   newArvore = adicionaFilho arvore subArvore
                                                                                   exemplosAtualizados = (map (removeItem (index)) exemplosi)

-- As funções mesmaClasse e mesmaClasse' são usadas para verificar se um conjunto de dados possui todas as classes iguais
mesmaClasse :: [[String]] -> Bool
mesmaClasse (x:xs) =  mesmaClasse' (last x) (x:xs) True             

mesmaClasse' :: String -> [[String]] -> Bool -> Bool
mesmaClasse' _ [] True = True
mesmaClasse' anterior (x:xs) igual  = if classe == anterior then mesmaClasse' classe xs True else False
                                    where classe =  last x

--cria um no com o melhor teste
raiz :: String -> String -> Int -> Arv
raiz melhor value indice = No {pergunta = melhor, filhos = [], resposta = value, index=indice}

--adiciona um filho à lista de filhos de um no
adicionaFilho :: Arv -> Arv -> Arv
adicionaFilho (No a filhos b i) subArvore = (No a (filhos ++ [subArvore]) b i)

--determina qual a classe que mais ocorre
maioria :: Eq t => [[t]] -> [t] -> t -> t
maioria [] classes maisComum = maisComum
maioria ex classes maisComum = maioria' ocorrencias (tail classes) (maximum ocorrencias)
                                   where ocorrencias = percentagens ex classes 

maioria' :: Eq t => [t] -> [p] -> t -> p
maioria' (o:os) (c:cs) maior = if maior == o then c else maioria' os cs maior

-- usado para classificar mais de um exemplo caso acontecesse de caso.txt tivesse mais casos
classificarTodos :: [[String]] -> Arv -> [String]
classificarTodos cs arv = [ (classificar x arv) | x <- cs ]

classificar :: [String] -> Arv -> String
classificar c (Folha classe' _) = classe'
classificar c (No b (a:as) d i) = if compararCaso (c!!i) value then classificar c a  else classificar c (No b as d i) 
                                          where value = resposta a
                             
-- remove um item de uma lista a partir de um indice
removeItem :: (Eq t, Num t) => t -> [a] -> [a]
removeItem 0 (y:ys) = ys
removeItem n (y:ys) = y : removeItem (n-1) ys

--usado para triplas
trd :: (a, b, c) -> c
trd (_,_,x) = x

fst' :: (a, b, c) -> a
fst' (x,_,_) = x

snd' :: (a, b, c) -> b
snd' (_,x,_) = x

--usado na hr de formatar a saida
indenta :: Int -> [Char]
indenta h = concat (replicate h "   ") 


--imprime a arovre no padrao dado pelo professor
imprimirArv :: Arv -> Int -> [Char]
imprimirArv (Folha cl res) h = "\n" ++ indenta h ++ "retorne " ++ cl 
imprimirArv (No p (f:[]) r qlqr) h = "\n" ++ (indenta h) ++ "se " ++ formata p (words (resposta f)) ++ " entao" ++ imprimirArv f (h+1) ++ "\n" ++ indenta h ++ "fim-se"
imprimirArv (No p (f:fs) r qlqr) h = "\n"++ (indenta h) ++ "se " ++ formata p (words (resposta f)) ++ " entao" ++ imprimirArv f (h+1) ++ "\n"++ indenta h++"senao" ++ imprimirArv(No p (fs) r qlqr) (h+1) ++ "\n" ++ indenta h ++ "fim-se"

formata :: [Char] -> [[Char]] -> [Char]
formata p parseValue        | head parseValue == "<=" =  p ++ " <= " ++ printa (parseValue!!1)
                            | head parseValue == "<>" = printa (parseValue!!1) ++ " < " ++ p ++ " <= " ++ printa (parseValue!!2)
                            | head parseValue == ">>" = p ++ " > " ++ printa (parseValue!!1)
                            | otherwise = p ++ " = " ++ (head parseValue)
  
--limita aas casas decimais de uma string
printa :: String -> String
printa p = printf "%.1f" num
            where num = read p :: Double

