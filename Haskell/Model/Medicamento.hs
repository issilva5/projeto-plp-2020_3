module Haskell.Model.Medicamento where

data Medicamento = Medicamento {
    id :: Int,
    idUbs :: Int,
    qtdEstoque :: Int,
    bula :: String
} deriving (Show)