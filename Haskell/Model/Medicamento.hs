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
    let id = read (l !! 1) :: Int
    let idUBS = read (l !! 0) :: Int
    let nome = l !! 2
    let qtdEstoque = read (l !! 3) :: Int 
    let bula = l !! 4
    [(Medicamento id idUBS nome qtdEstoque bula, "")]

instance Eq Medicamento where
    (Medicamento _ _ _ q1 _) == (Medicamento _ _ _ q2 _) = q1 == q2

instance Ord Medicamento where
    compare (Medicamento _ _ _ q1 _) (Medicamento _ _ _ q2 _) = compare q1 q2