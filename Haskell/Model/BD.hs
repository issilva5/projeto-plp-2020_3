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
    logins :: [(Int, String)]
} deriving (Show)