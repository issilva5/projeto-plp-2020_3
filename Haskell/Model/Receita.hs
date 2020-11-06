module Haskell.Model.Receita where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data Receita = Receita {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    remedios :: [(Int, String, Int)]
} deriving (Show)

toString :: Receita -> String
toString r =
    show (id r) ++ ";" ++
    show (idPaciente r) ++ ";" ++
    show (idMedico r) ++ ";" ++
    show (idUBS r) ++ ";" ++
    show (remedios r)

instance Read Receita where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let idPaciente = read (l !! 1) :: Int
    let idMedico = read (l !! 2) :: Int
    let idUBS = read (l !! 3) :: Int
    let remedios = read (l !! 4) :: [(Int, String, Int)]
    [(Receita id idPaciente idMedico idUBS remedios, "")]