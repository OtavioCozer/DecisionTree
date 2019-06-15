import Leitura
import Func


entrada ="aparencia sol chuva nublado\n eu n sei oq eu to fazendo"


entrada1 = "Aparencia Sol Chuva Nublado\nTemperatura\nUmidade\nVento Sim Nao\nViajar Va NaoVa"

data Arv = Nil | Nos [String] Arv | Non [Float] Arv deriving Show

cria x = Non x Nil 