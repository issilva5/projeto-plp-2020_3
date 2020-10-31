module Haskell.Model.Medicamento where
import Haskell.View.Utils (split) 

data Medicamento = Medicamento {
    id :: Int,
    idUBS :: Int,
    nome :: String,
    qtdEstoque :: Int,
    bula :: String
} deriving (Show)

instance Read Medicamento where 
    readsPrec _ str = do 
    let l = split str ';' ""
    let id = read (l !! 0) :: Int 
    let qtdEstoque = read (l !! 1) :: Int 
    let bula = l !! 2
    [(Medicamento id qtdEstoque bula, "")]