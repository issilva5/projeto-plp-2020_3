module Haskell.Model.Consulta where

import Data.Dates

data Consulta = Consulta {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    dia :: DateTime
} deriving (Show)