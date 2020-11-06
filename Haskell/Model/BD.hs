module Haskell.Model.BD where

import qualified Haskell.Model.Paciente as Paciente
import qualified Haskell.Model.Medico as Medico
import qualified Haskell.Model.UBS as UBS
import qualified Haskell.Model.Consulta as Consulta
import qualified Haskell.Model.Exame as Exame
import qualified Haskell.Model.Laudo as Laudo
import qualified Haskell.Model.Medicamento as Medicamento
import qualified Haskell.Model.Receita as Receita

data BD = BD {
    pacientes :: [Paciente.Paciente],
    medicos :: [Medico.Medico],
    ubs :: [UBS.UBS],
    consultas :: [Consulta.Consulta],
    exames :: [Exame.Exame],
    laudos :: [Laudo.Laudo],
    medicamentos :: [Medicamento.Medicamento],
    receitas :: [Receita.Receita],
    logins :: [(Int, String, Int)],
    idAtual :: Int
} deriving (Show)

pacientesToString :: [Paciente.Paciente] -> String -> String
pacientesToString [] aux = aux
pacientesToString (h:t) aux = aux ++ (Paciente.toString h) ++ "\n" ++ pacientesToString t aux

stringToPaciente :: [String] -> [Paciente.Paciente]
stringToPaciente l = map read l :: [Paciente.Paciente]

ubsToString :: [UBS.UBS] -> String -> String
ubsToString [] aux = aux
ubsToString (h:t) (aux) = aux ++ (UBS.toString h) ++ "\n" ++ ubsToString t aux

stringToUBS :: [String] -> [UBS.UBS]
stringToUBS l = map read l :: [UBS.UBS]

loginsToString :: [(Int, String, Int)] -> String -> String
loginsToString [] aux = aux
loginsToString (h:t) aux = aux ++ (show h) ++ "\n" ++ loginsToString t aux

stringToLogin :: [String] -> [(Int, String, Int)]
stringToLogin l = map read l :: [(Int, String, Int)]

medicosToString :: [Medico.Medico] -> String -> String
medicosToString [] aux = aux
medicosToString (h:t) aux = aux ++ (Medico.toString h) ++ "\n" ++ medicosToString t aux

stringToMedico :: [String] -> [Medico.Medico]
stringToMedico l = map read l :: [Medico.Medico]

medicamentosToString :: [Medicamento.Medicamento] -> String -> String
medicamentosToString [] aux = aux
medicamentosToString (h:t) aux = aux ++ (Medicamento.toString h) ++ "\n" ++ medicamentosToString t aux

stringToMedicamento :: [String] -> [Medicamento.Medicamento]
stringToMedicamento l = map read l :: [Medicamento.Medicamento]

receitasToString :: [Receita.Receita] -> String -> String
receitasToString [] aux = aux
receitasToString (h:t) aux = aux ++ (Receita.toString h) ++ "\n" ++ receitasToString t aux

stringToReceita :: [String] -> [Receita.Receita]
stringToReceita l = map read l :: [Receita.Receita]

consultasToString :: [Consulta.Consulta] -> String -> String
consultasToString [] aux = aux
consultasToString (h:t) aux = aux ++ (Consulta.toString h) ++ "\n" ++ consultasToString t aux

stringToConsulta :: [String] -> [Consulta.Consulta]
stringToConsulta l = map read l :: [Consulta.Consulta]

examesToString :: [Exame.Exame] -> String -> String
examesToString [] aux = aux
examesToString (h:t) aux = aux ++ (Exame.toString h) ++ "\n" ++ examesToString t aux

stringToExame :: [String] -> [Exame.Exame]
stringToExame l = map read l :: [Exame.Exame]

laudosToString :: [Laudo.Laudo] -> String -> String
laudosToString [] aux = aux
laudosToString (h:t) aux = aux ++ (Laudo.toString h) ++ "\n" ++ laudosToString t aux

stringToLaudo :: [String] -> [Laudo.Laudo]
stringToLaudo l = map read l :: [Laudo.Laudo]

-- consultas
-- exames
-- laudos
-- receitas

{-
nextID :: BD -> Int
nextID bd = head (idAtual bd) -}
