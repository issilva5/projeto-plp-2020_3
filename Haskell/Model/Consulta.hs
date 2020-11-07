module Haskell.Model.Consulta where
import Prelude hiding (id)
import Haskell.View.Utils

import Data.Dates

data Consulta = Consulta {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    dia :: DateTime
}

toString :: Consulta -> String
toString c =
    show (id c) ++ ";" ++
    show (idPaciente c) ++ ";" ++
    show (idMedico c) ++ ";" ++
    show (idUBS c) ++ ";" ++
    dateTimeToString (dia c)

instance Show Consulta where
    show (Consulta id idP idM idU date) = "----------------------------\n" ++
                                          "CONSULTA " ++ (show id) ++ "\n" ++
                                          "Paciente: " ++ (show idP) ++ "\n" ++
                                          "Médico responsável: " ++ (show idM) ++ "\n" ++
                                          "UBS: " ++ (show idU) ++ "\n" ++
                                          "Data: " ++ (dateTimeToString date)

formataConsulta :: Consulta -> String
formataConsulta c = "Consulta " ++ (show (id c)) ++ " do paciente " ++ (show (idPaciente c)) ++ " com o médico " ++ (show (idMedico c)) ++ " em " ++ (show (dia c))

instance Read Consulta where 
    readsPrec _ str = do 
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let idPaciente = read (l !! 1) :: Int
    let idMedico = read (l !! 2) :: Int
    let idUBS = read (l !! 3) :: Int
    let dia = read (l !! 4) :: DateTime
    [(Consulta id idPaciente idMedico idUBS dia, "")]
