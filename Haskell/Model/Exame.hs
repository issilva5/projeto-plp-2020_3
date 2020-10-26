module Haskell.Model.Exame where

data Exame = Exame {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    tipo :: String,
    dia :: String,
    resultado :: String
} deriving (Show)