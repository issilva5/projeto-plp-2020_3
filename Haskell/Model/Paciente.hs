module Haskell.Model.Paciente where
import Haskell.View.Utils (split) 

data Paciente = Paciente {
    id :: Int,
    nome :: String,
    cpf :: String,
    dataNascimento :: String,
    peso :: Double,
    altura :: Double,
    tipoSanguineo :: String,
    endereco :: String,
    cardiopata :: Bool,
    diabetico :: Bool,
    hipertenso :: Bool
} deriving (Show)

instance Read Paciente where 
    readsPrec _ str = do 
    let l = split str ';' ""
    let id = read (l !! 0) :: Int 
    let nome = l !! 1 
    let cpf = l !! 2
    let dataNascimento = l !! 3
    let peso = read (l !! 4) :: Double 
    let altura = read ( l !! 5) :: Double
    let tipoSanguineo = l !! 6
    let endereco = l !! 7
    let cardiopata = read (l !! 8) :: Bool
    let diabetico =  read (l !! 9) :: Bool
    let hipertenso = read (l !! 10) :: Bool
    [((Paciente id nome cpf dataNascimento peso altura tipoSanguineo endereco cardiopata diabetico hipertenso), "")]