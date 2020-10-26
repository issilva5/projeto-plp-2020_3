module Haskell.Model.Medicamento where

data Medicamento = Medicamento {
    id :: Int,
    qtdEstoque :: Int,
    bula :: String
} deriving (Show)