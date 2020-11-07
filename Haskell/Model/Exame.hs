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
}

toString :: Exame -> String
toString e =
    show (id e) ++ ";" ++
    show (idPaciente e) ++ ";" ++
    show (idMedico e) ++ ";" ++
    show (idUBS e) ++ ";" ++
    show (tipo e) ++ ";" ++
    dateTimeToString (dia e) ++ ";" ++
    show (resultado e)

instance Show Exame where
    show (Exame id idP idM idU t d r) = "----------------------------\n" ++
                                        "EXAME " ++ (show id) ++ "\n" ++
                                        "Paciente: " ++ (show idP) ++ "\n" ++
                                        "Médico responsável: " ++ (show idM) ++ "\n" ++
                                        "UBS: " ++ (show idU) ++ "\n" ++
                                        "Tipo do exame: " ++ t ++ "\n" ++
                                        "Data: " ++ (dateTimeToString d) ++ "\n" ++
                                        "Resultado: " ++ r

instance Read Exame where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let idPaciente = read (l !! 1) :: Int
    let idMedico = read (l !! 2) :: Int
    let idUBS = read (l !! 3) :: Int
    let tipo = l !! 4
    let dia = read (l !! 5) :: DateTime
    let resultado = l !! 6
    [(Exame id idPaciente idMedico idUBS tipo dia resultado, "")]