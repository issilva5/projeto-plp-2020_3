module Haskell.Controller.MedicoController (
  informarHorario,
  acessarDadosPaciente,
  acessarConsultas,
  acessarConsultasData,
  emitirReceita,
  emitirExame,
  emitirLaudo,
  acessarExames,
  acessarExame,
  solicitarTransferencia
) where

import Haskell.Model.Paciente
import Haskell.Model.Consulta
import Haskell.Model.Receita
import Haskell.Model.Exame
import Haskell.Model.Laudo
import Data.Dates

-- informa meu horario de atendimento no formato 'Ds HH:MM a HH:MM'
-- retorno: booleano confirmando se deu certo
informarHorario :: Int -> String -> Bool
informarHorario idMed horario = False

-- descricao: pegar os dados do paciente
-- retorno: paciente
-- possivelmente reaproveitar a do PacienteController
acessarDadosPaciente :: [Paciente] -> Int -> Paciente
acessarDadosPaciente _ idPaciente = (Paciente 1 "" "" "" 80.00 1.50 "" "" False False False)

-- descricao: pegar os dados das consultas que tenho agendadas
acessarConsultas :: Int -> [Consulta] -> [Consulta]
acessarConsultas idMed consultas = []

-- descricao: pegar os dados das consultas que tenho agendadas
acessarConsultasData :: Int -> String -> [Consulta] -> [Consulta]
acessarConsultasData idMed date consultas = []

emitirReceita :: Int -> Int -> [String] -> Receita
emitirReceita idMed idPac informacoes = (Receita 1 1 1 1 [(1, "")])

emitirExame :: Int -> Int -> [String] -> Exame
emitirExame idMed idPac informacoes = (Exame 1 1 1 1 "" (DateTime 2020 10 30 00 00 00) "")

emitirLaudo :: Int -> Int -> [String] -> Laudo
emitirLaudo idMed idExame informacoes = (Laudo 1 1 "")

acessarExames :: Int -> [Exame] -> [Exame]
acessarExames idMed exames = []

acessarExame :: Int -> Int -> [Exame] -> Exame
acessarExame idMed idExame exames = (Exame 1 1 1 1 "" (DateTime 2020 10 30 00 00 00) "")

solicitarTransferencia :: Int -> Int -> Bool
solicitarTransferencia idMed idUBS = False
