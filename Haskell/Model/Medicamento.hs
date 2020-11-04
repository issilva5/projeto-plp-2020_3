module Haskell.Model.Medicamento where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data Medicamento = Medicamento {
    id :: Int,
    idUBS :: Int,
    nome :: String,
    qtdEstoque :: Int,
    bula :: String
} deriving (Show)

toString :: Medicamento -> String
toString m =
    show (id m) ++ ";" ++
    show (idUBS m) ++ ";" ++
         (nome m) ++ ";" ++
    show (qtdEstoque m) ++ ";" ++
         (bula m)

instance Read Medicamento where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 1) :: Int
    let idUBS = read (l !! 0) :: Int
    let nome = read (l !! 2) :: String
    let qtdEstoque = read (l !! 3) :: Int
    let bula = read (l !! 4) :: String
    [(Medicamento id idUBS nome qtdEstoque bula, "")]