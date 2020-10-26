module Haskell.Model.UBS where

data UBS = UBS {
    id :: Int,
    nome :: String,
    endereco :: String
} deriving (Show, Eq)
