module Haskell.Model.UBS where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data UBS = UBS {
    id :: Int,
    nome :: String,
    endereco :: String
} deriving (Show)

toString :: UBS -> String
toString u =
    show (id u) ++ ";" ++
         (nome u) ++ ";" ++
         (endereco u)

instance Read UBS where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let nome = read (l !! 1) :: String
    let endereco = read (l !! 2) :: String
    [(UBS id nome endereco, "")]