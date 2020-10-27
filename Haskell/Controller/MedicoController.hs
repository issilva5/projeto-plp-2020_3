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

-- informa meu horario de atendimento no formato 'Ds HH:MM a HH:MM'
-- retorno: booleano confirmando se deu certo
informarHorario :: Int -> String -> Bool
informarHorario idMed horario = False

-- descricao: pegar os dados do paciente
-- retorno: paciente
acessarDadosPaciente :: Int -> Int -> Paciente
acessarDadosPaciente idMed idPaciente = (Paciente 1 "" "" "" 80.00 1.50 "" "" False False False ["Poeira"])

-- descricao: pegar os dados das consultas que tenho agendadas
acessarConsultas :: Int -> [Consulta] -> [Consulta]
acessarConsultas idMed consultas = []

-- descricao: pegar os dados das consultas que tenho agendadas
acessarConsultasData :: Int -> String -> [Consulta] -> [Consulta]
acessarConsultasData idMed date consultas = []

-- O que danado é ????
emitirReceita :: Int -> Int -> Receita
emitirReceita idMed idPac = (Receita 1 1 1 1 [(1, "")])

emitirExame :: Int -> Int -> Receita
emitirExame idMed idPac = (Receita 1 1 1 1 [(1, "")])

emitirLaudo :: Int -> Int -> Receita
emitirLaudo idMed idExame = (Receita 1 1 1 1 [(1, "")])

acessarExames :: Int -> [Exame] -> [Exame]
acessarExames idMed exame = []

acessarExame :: Int -> Int -> Exame
acessarExame idMed idExame = (Exame 1 1 1 1 "" "" "")

solicitarTransferencia :: Int -> Int -> Bool
solicitarTransferencia idMed idUBS = False