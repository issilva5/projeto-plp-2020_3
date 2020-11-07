module Haskell.Model.Medicamento where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data Medicamento = Medicamento {
    id :: Int,
    idUBS :: Int,
    nome :: String,
    qtdEstoque :: Int,
    bula :: String
}

toString :: Medicamento -> String
toString m =
    show (id m) ++ ";" ++
    show (idUBS m) ++ ";" ++
         (nome m) ++ ";" ++
    show (qtdEstoque m) ++ ";" ++
         (bula m)

instance Show Medicamento where
    show (Medicamento id idU n qtd b) = "----------------------------\n" ++
                                        "MEDICAMENTO " ++ (show id) ++ "\n" ++
                                        "Nome: " ++ n ++ "\n" ++
                                        "UBS: " ++ (show idU) ++ "\n" ++
                                        "Quantidade em estoque: " ++ (show qtd) ++ "\n" ++
                                        "Bula: " ++ b

instance Read Medicamento where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 1) :: Int
    let idUBS = read (l !! 0) :: Int
    let nome = l !! 2
    let qtdEstoque = read (l !! 3) :: Int
    let bula = l !! 4
    [(Medicamento id idUBS nome qtdEstoque bula, "")]