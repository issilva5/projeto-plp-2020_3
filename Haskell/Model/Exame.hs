module Haskell.Model.Exame where
import Haskell.View.Utils (split) 

import Data.Dates

data Exame = Exame {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    tipo :: String,
    dia :: DateTime,
    resultado :: String
} deriving (Show)

instance Read Exame where 
    readsPrec _ str = do 
    let l = split str ';' ""
    let id = read (l !! 0) :: Int 
    let idPaciente = read (l !! 1) :: Int
    let idMedico = read (l !! 2) :: Int
    let idUBS = read (l !! 3) :: Int
    let tipo = l !! 4
    let dia = DateTime 2020 11 01 00 00 00
    let resultado = ""
    [(Exame id idPaciente idMedico idUBS tipo dia resultado, "")]