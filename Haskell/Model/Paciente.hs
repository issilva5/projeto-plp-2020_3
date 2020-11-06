module Haskell.Model.Paciente where
import Haskell.View.Utils (split)
import Prelude hiding (id)
import Data.Char (toUpper)

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
                  (nome p) ++ ";" ++
                  (cpf p) ++ ";" ++
                  (dataNascimento p) ++ ";" ++
             show (peso p) ++ ";" ++
             show (altura p) ++ ";" ++
                  (tipoSanguineo p) ++ ";" ++
                  (endereco p) ++ ";" ++
             show (cardiopata p) ++ ";" ++
             show (diabetico p) ++ ";" ++
             show (hipertenso p) ++ ";"

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
    let cardiopata = if (toUpper $ head (l !! 8)) == 'S' then True else False
    let diabetico =  if (toUpper $ head (l !! 9)) == 'S' then True else False
    let hipertenso = if (toUpper $ head (l !! 10)) == 'S' then True else False
    [((Paciente id nome cpf dataNascimento peso altura tipoSanguineo endereco cardiopata diabetico hipertenso), "")]