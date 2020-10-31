module Haskell.Model.Medico where
import Haskell.View.Utils (split) 

data Medico = Medico {
    id :: Int,
    nome :: String,
    crm :: String,
    idUbs :: Int,
    especialidade :: String,
    horarios :: [String]
} deriving (Show)

instance Read Medico where 
    readsPrec _ str = do 
    let l = split str ';' ""   
    let id = read (l !! 0) :: Int
    let nome = l !! 1
    let crm = l !! 2
    let idUbs = read (l !! 3) :: Int
    let especialidade = l !! 4
    let horarios = split (l !! 5) ',' ""    
    [(Medico id nome crm idUbs especialidade horarios, "")]