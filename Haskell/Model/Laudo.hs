module Haskell.Model.Laudo where
data Laudo = Laudo {
     id :: Int,
     idExame :: Int,
     texto :: String
} deriving (Show)