module Haskell.Model.Laudo where
import Haskell.View.Utils (split) 

data Laudo = Laudo {
     id :: Int,
     idExame :: Int,
     texto :: String
} deriving (Show)

instance Read Laudo where 
     readsPrec _ str = do 
     let l = split str ';' ""
     let id = read (l !! 0) :: Int
     let idExame = read (l !! 1) :: Int
     let texto = l !! 2

     [(Laudo id idExame texto, "")]