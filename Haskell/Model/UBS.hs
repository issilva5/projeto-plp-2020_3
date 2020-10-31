module Haskell.Model.UBS where
import Haskell.View.Utils (split) 

data UBS = UBS {
    id :: Int,
    nome :: String,
    endereco :: String
} deriving (Show)

instance Read UBS where 
    readsPrec _ str = do 
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let nome = l !! 1
    let endereco = l !! 2
    [(UBS id nome endereco, "")]