import Leitura
import Func

data Arv =   No {pergunta :: String, filhos :: [Arv], resposta :: String} 
          | Folha{classe :: String, resposta :: String} deriving Show


main = do
            nome <- readFile "descricao.txt"
            putStrLn  $ show (leEntrada nome)

-- As funções mesmaClasse e mesmaClasse' são usadas para verificar se um conjunto de dados possui todas as classes iguais
mesmaClasse :: [[String]] -> Bool
mesmaClasse (x:xs) = mesmaClasse' (last x) (x:xs) True             

mesmaClasse' :: String -> [[String]] -> Bool -> Bool
mesmaClasse' _ [] True = True
mesmaClasse' anterior (x:xs) igual  = if classe == anterior then mesmaClasse' classe xs True else False
                                    where classe =  last x







teste = No {pergunta = "otavio", filhos = [], resposta = "n sei"}

entrada ="aparencia sol chuva nublado\n eu n sei oq eu to fazendo"

entrada1 = "Aparencia Sol Chuva Nublado\nTemperatura\nUmidade\nVento Sim Nao\nViajar Va NaoVa"

base = [["Sol","25","72","Sim","Va"],["Sol","28","91","Sim","NaoVa"],["Sol","22","70","Nao","Va"],["Sol","23","95","Nao","NaoVa"],["Sol","30","85","Nao","NaoVa"]]

base1 = [["Sol","25","72","Sim","Va"]]

descricao = [["Aparencia","Sol","Chuva","Nublado"],["Temperatura"],["Umidade"],["Vento","Sim","Nao"],["Viajar","Va","NaoVa"]]