import Leitura
import Func

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String} 
          | Folha{classe :: String, resposta :: String} deriving Show


main = do
            nome <- readFile "base.txt"
            putStrLn  $ show (leEntrada nome)

-- As funções mesmaClasse e mesmaClasse' são usadas para verificar se um conjunto de dados possui todas as classes iguais
mesmaClasse :: [[String]] -> Bool
mesmaClasse (x:xs) = mesmaClasse' (last x) (x:xs) True             

mesmaClasse' :: String -> [[String]] -> Bool -> Bool
mesmaClasse' _ [] True = True
mesmaClasse' anterior (x:xs) igual  = if classe == anterior then mesmaClasse' classe xs True else False
                                    where classe =  last x







caracteristicas1 = [["Aparencia" , "Sol" , "Chuva" , "Nublado"] ,["Temperatura"], ["Umidade"], ["Vento", "Sim", "Nao"]]

classes1 = ["Viajar", "Va", "NaoVa"]
                                    
ex1 = [["Sol","25","72","Sim","Va"],
       ["Sol","28","91","Sim","NaoVa"],
       ["Sol","22","70","Nao","Va"],
       ["Sol","23","95","Nao","NaoVa"],
       ["Sol","30","85","Nao","NaoVa"]] 
                                    