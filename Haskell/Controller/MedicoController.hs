module Haskell.Controller.MedicoController (
  informarHorario,
  acessarDadosPaciente,
  acessarConsultas,
  acessarConsultasData,
  emitirReceita,
  emitirResultadoExame,
  emitirLaudo,
  acessarExames,
  acessarExame,
  solicitarTransferencia,
  proximoHorarioLivre,
  acessarExamesPaciente,
  getUBS,
  validaIDMedico
) where

import Haskell.Model.Paciente
import Haskell.Model.Consulta
import Haskell.Model.Receita
import Haskell.Model.Medico
import Haskell.Model.Exame
import Haskell.Model.Laudo
import Data.Dates
import Data.List ( intercalate ) 

{-

Cria os horários para o médico.
@param idMed: id do médico a ter o horário definido
@param horarioIni: lista de tamanho 7 dos horários de início de expediente
@param horarioFim: lista de tamanho 7 dos horários de fim de expediente
@param duracao: duração da consulta que o médico oferece
@param medicos: lista de médicos
@return lista de médicos com o médico dado alterado

-}
informarHorario :: Int -> [Time] -> [Time] -> Int -> [Medico] -> [Medico]
informarHorario idMed horarioIni horarioFim duracao medicos = []

{-

Acessar dados de um paciente.
@param pacientes: lista dos pacientes
@param idPaciente: id do paciente
@return paciente procurado

-}
acessarDadosPaciente :: [Paciente] -> Int -> Paciente
acessarDadosPaciente pacientes idPaciente = (Paciente 1 "" "" "" 80.00 1.50 "" "" False False False)

{-

Acessar as consultas que me foram alocados.
@param idMed: id do médico
@param consultas: lista de consultas
@return lista de consultas do médico

-}
acessarConsultas :: Int -> [Consulta] -> [Consulta]
acessarConsultas idMed consultas = []

{-

Acessar as consultas que me foram alocados por data.
@param idMed: id do médico
@param date: data
@return lista de consultas do médico na data

-}
acessarConsultasData :: Int -> DateTime -> [Consulta] -> [Consulta]
acessarConsultasData idMed date consultas = []

{-

Emite uma receita
@param id: id da Receita
@param idMed: id do médico emissor
@param idPac: id do paciente
@param idUBS: id da UBS onde o médico trabalha
@param informacoes: dados sobre os remédios da receita
@return receita criada

-}
emitirReceita :: Int -> Int -> Int -> Int -> [String] -> Receita
emitirReceita id idMed idPac idUBS informacoes = read (intercalate ";" ([show id, show idPac, show idMed, show idUBS] ++ informacoes))

{-

Adiciona o resultado de um exame.
@param exames: lista de exames
@param idExame: id do exame a ter o resultado adicionado
@param resultado: resultado do exame
@return lista de exames com o exame alterado

-}
emitirResultadoExame :: [Exame] -> Int -> String -> [Exame]
emitirResultadoExame exames idExame resultado = []

{-

Emite um laudo.
@param id: id do laudo
@param idMed: id do médico emissor
@param idExame: id do exame associado
@param informacoes: texto do laudo

-}
emitirLaudo :: Int -> Int -> Int -> [String] -> Laudo
emitirLaudo id idMed idExame informacoes = read (intercalate ";" ([show id, show idMed, show idExame] ++ informacoes))

{-

Acessar os exames que me foram alocados.
@param idMed: id do médico
@param exames: lista de exames
@return lista de exames do médico

-}
acessarExames :: Int -> [Exame] -> [Exame]
acessarExames idMed exames = []

{-

Acessar um exame que me foi alocado.
@param idExame: id do exame
@param exames: lista de exames
@return exame

-}
acessarExame :: Int -> [Exame] -> Exame
acessarExame idExame exames = (Exame 1 1 1 1 "" (DateTime 2020 10 30 00 00 00) "")

{-

Acessar exames de um paciente.
@param idExame: id do paciente
@param exames: lista de exames
@return lista de exames do paciente

-}
acessarExamesPaciente :: Int -> [Exame] -> [Exame]
acessarExamesPaciente idPaciente exames = []

{-

Altera a unidade de saúde do médico
@param idMedico: id do médico a ser transferido
@param idUBS: nova ubs
@param medicos: lista dos medicos
@return lista dos medicos com o medico atualizado

-}
solicitarTransferencia :: Int -> Int -> [Medico] -> [Medico]
solicitarTransferencia idMed idUBS medicos = []

{-

Retorna o id da UBS de um médico.
@param idMed: id do médico
@param medicos: lista de médicos
@return id da UBS onde o médico esta alocado

-}
getUBS :: Int -> [Medico] -> Int
getUBS idMed medicos = 0

{-

Retorna o próximo horário livre do médico e a lista de médicos alterada
@param idMed: id do médico
@param medicos: lista de medicos
@return (horário livre, lista de medicos com o horario livre do medico alterado)

-}
proximoHorarioLivre :: Int -> [Medico] -> (DateTime, [Medico])
proximoHorarioLivre idMed medicos = ((DateTime 2020 10 30 00 00 00), [])

{-

Verifica se existe médico com o id dado
@param idMed: id do médico
@param medicos: lista dos médicos
@return True se existir, False c.c.

-}
validaIDMedico :: Int -> [Medico] -> Bool
validaIDMedico idMed medicos = True