module Haskell.Model.Laudo where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data Laudo = Laudo {
     id :: Int,
     idMed :: Int,
     idExame :: Int,
     texto :: String
} deriving (Show)

toString :: Laudo -> String
toString l =
     show (id l) ++ ";" ++
     show (idMed l) ++ ";" ++
     show (idExame l) ++ ";" ++
     texto l

instance Read Laudo where
     readsPrec _ str = do
     let l = split str ';' ""
     let id = read (l !! 0) :: Int
     let idMed = read (l !! 1) :: Int
     let idExame = read (l !! 2) :: Int
     let texto = read (l !! 3) :: String

     [(Laudo id idMed idExame texto, "")]