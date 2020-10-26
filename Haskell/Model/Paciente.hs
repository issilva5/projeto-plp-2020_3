module Haskell.Model.Paciente where

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
    hipertenso :: Bool,
    alergias :: [String]
} deriving (Show)