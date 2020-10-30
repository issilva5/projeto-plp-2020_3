module Haskell.Model.Exame where

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
