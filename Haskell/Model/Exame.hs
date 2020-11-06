module Haskell.Model.Exame where
import Haskell.View.Utils
import Prelude hiding (id)
import Data.Dates

data Exame = Exame {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    tipo :: String,
    dia :: DateTime,
    resultado :: String
} deriving (Show)

toString :: Exame -> String
toString e =
    show (id e) ++ ";" ++
    show (idPaciente e) ++ ";" ++
    show (idMedico e) ++ ";" ++
    show (idUBS e) ++ ";" ++
         (tipo e) ++ ";" ++
    dateTimeToString (dia e)

instance Read Exame where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let idPaciente = read (l !! 1) :: Int
    let idMedico = read (l !! 2) :: Int
    let idUBS = read (l !! 3) :: Int
    let tipo = l !! 4
    let dia = read (l !! 5) :: DateTime
    let resultado = ""
    [(Exame id idPaciente idMedico idUBS tipo dia resultado, "")]