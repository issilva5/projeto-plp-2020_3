module Haskell.Model.Medico where
import Haskell.View.Utils (split) 

import Haskell.Model.DateCycle

data Medico = Medico {
    id :: Int,
    nome :: String,
    crm :: String,
    idUbs :: Int,
    especialidade :: String,
    horarios :: DateCycle
} deriving (Show)

instance Read Medico where 
    readsPrec _ str = do 
    let l = split str ';' ""   
    let id = read (l !! 1) :: Int
    let nome = l !! 2
    let crm = l !! 3
    let idUbs = read (l !! 0) :: Int
    let especialidade = l !! 4
    let horarios = empty    
    [(Medico id nome crm idUbs especialidade horarios, "")]
