module Haskell.Model.Paciente where
import Haskell.View.Utils (split)
import Prelude hiding (id)

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

toString :: Paciente -> String
toString p = show (id p) ++ ";" ++
             show (nome p) ++ ";" ++
             show (cpf p) ++ ";" ++
             show (dataNascimento p) ++ ";" ++
             show (peso p) ++ ";" ++
             show (altura p) ++ ";" ++
             show (tipoSanguineo p) ++ ";" ++
             show (endereco p) ++ ";" ++
             show (cardiopata p) ++ ";" ++
             show (diabetico p) ++ ";" ++
             show (hipertenso p) ++ ";"

instance Read Paciente where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let nome = read (l !! 1) :: String
    let cpf = read (l !! 2) :: String
    let dataNascimento = read (l !! 3) :: String
    let peso = read (l !! 4) :: Double
    let altura = read ( l !! 5) :: Double
    let tipoSanguineo = read (l !! 6)
    let endereco = read (l !! 7) :: String
    let cardiopata = read (l !! 8) :: Bool
    let diabetico =  read (l !! 9) :: Bool
    let hipertenso = read (l !! 10) :: Bool
    [((Paciente id nome cpf dataNascimento peso altura tipoSanguineo endereco cardiopata diabetico hipertenso), "")]