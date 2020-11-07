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

import qualified Haskell.Model.Paciente as Paciente
import qualified Haskell.Model.Consulta as Consulta
import qualified Haskell.Model.Receita as Receita
import qualified Haskell.Model.Medico as Medico
import qualified Haskell.Model.Exame as Exame
import qualified Haskell.Model.Laudo as Laudo
import Data.Dates
import Data.List ( intercalate )
import Haskell.Model.DateCycle (newDC, (===), getNextDate)

{-

Cria os horários para o médico.
@param idMed: id do médico a ter o horário definido
@param horarioIni: lista de tamanho 7 dos horários de início de expediente
@param horarioFim: lista de tamanho 7 dos horários de fim de expediente
@param duracao: duração da consulta que o médico oferece
@param medicos: lista de médicos
@return lista de médicos com o médico dado alterado

-}
informarHorario :: Int -> DateTime -> [Time] -> [Time] -> Int -> [Medico.Medico] -> [Medico.Medico]
informarHorario _ _ _ _ _ [] = []
informarHorario idMed hj horarioIni horarioFim duracao (x:xs) | idMed == (Medico.id x) = [x {Medico.horarios = (newDC hj horarioIni horarioFim duracao)}] ++ (informarHorario idMed hj horarioIni horarioFim duracao xs)
                                                              | otherwise = [x] ++ (informarHorario idMed hj horarioIni horarioFim duracao xs)

{-

Acessar dados de um paciente.
@param pacientes: lista dos pacientes
@param idPaciente: id do paciente
@return paciente procurado ou Nothing c.c.

-}
acessarDadosPaciente :: [Paciente.Paciente] -> Int -> Maybe Paciente.Paciente
acessarDadosPaciente [] _ = Nothing
acessarDadosPaciente (x:xs) idPaciente | idPaciente == (Paciente.id x) = Just x
                                       | otherwise = acessarDadosPaciente xs idPaciente

{-

Acessar as consultas que me foram alocados.
@param idMed: id do médico
@param consultas: lista de consultas
@return lista de consultas do médico

-}
acessarConsultas :: Int -> [Consulta.Consulta] -> [Consulta.Consulta]
acessarConsultas _ [] = []
acessarConsultas idMed (x:xs) | idMed == (Consulta.idMedico x) = [x] ++ (acessarConsultas idMed xs)
                              | otherwise = (acessarConsultas idMed xs)

{-

Acessar as consultas que me foram alocados por data.
@param idMed: id do médico
@param date: data
@return lista de consultas do médico na data

-}
acessarConsultasData :: Int -> DateTime -> [Consulta.Consulta] -> [Consulta.Consulta]
acessarConsultasData _ _ [] = []
acessarConsultasData idMed date (x:xs) | (idMed == (Consulta.idMedico x)) && ((Consulta.dia x) === date) = [x] ++ (acessarConsultas idMed xs)
                                       | otherwise = (acessarConsultasData idMed date xs)

{-

Emite uma receita
@param id: id da Receita
@param idMed: id do médico emissor
@param idPac: id do paciente
@param idUBS: id da UBS onde o médico trabalha
@param informacoes: dados sobre os remédios da receita
@return receita criada

-}
emitirReceita :: Int -> Int -> Int -> Int -> [String] -> Receita.Receita
emitirReceita id idMed idPac idUBS informacoes = read (intercalate ";" ([show id, show idPac, show idMed, show idUBS] ++ informacoes))

{-

Adiciona o resultado de um exame.
@param exames: lista de exames
@param idExame: id do exame a ter o resultado adicionado
@param resultado: resultado do exame
@return lista de exames com o exame alterado

-}
emitirResultadoExame :: [Exame.Exame] -> Int -> String -> [Exame.Exame]
emitirResultadoExame [] _ _ = []
emitirResultadoExame (x:xs) idExame resultado | idExame == (Exame.id x) = [x {Exame.resultado = resultado}] ++ (emitirResultadoExame xs idExame resultado)
                                              | otherwise = [x] ++ (emitirResultadoExame xs idExame resultado)

{-

Emite um laudo.
@param id: id do laudo
@param idMed: id do médico emissor
@param idExame: id do exame associado
@param informacoes: texto do laudo

-}
emitirLaudo :: Int -> Int -> Int -> [String] -> Laudo.Laudo
emitirLaudo id idMed idExame informacoes = read (intercalate ";" ([show id, show idMed, show idExame] ++ informacoes))

{-

Acessar os exames que me foram alocados.
@param idMed: id do médico
@param exames: lista de exames
@return lista de exames do médico

-}
acessarExames :: Int -> [Exame.Exame] -> [Exame.Exame]
acessarExames _ [] = []
acessarExames idMed (x:xs) | idMed == (Exame.idMedico x) = [x] ++ (acessarExames idMed xs)
                           | otherwise = (acessarExames idMed xs)

{-

Acessar um exame que me foi alocado.
@param idExame: id do exame
@param exames: lista de exames
@return exame

-}
acessarExame :: Int -> [Exame.Exame] -> Maybe Exame.Exame
acessarExame _ [] = Nothing
acessarExame idExame (x:xs) | idExame == (Exame.id x) = Just x
                            | otherwise = acessarExame idExame xs


{-

Acessar exames de um paciente.
@param idPac: id do paciente
@param exames: lista de exames
@return lista de exames do paciente

-}
acessarExamesPaciente :: Int -> [Exame.Exame] -> [Exame.Exame]
acessarExamesPaciente _ [] = []
acessarExamesPaciente idPac (x:xs) | idPac == (Exame.idPaciente x) = [x] ++ (acessarExamesPaciente idPac xs)
                                   | otherwise = (acessarExamesPaciente idPac xs)

{-

Altera a unidade de saúde do médico
@param idMedico: id do médico a ser transferido
@param idUBS: nova ubs
@param medicos: lista dos medicos
@return lista dos medicos com o medico atualizado

-}
solicitarTransferencia :: Int -> Int -> [Medico.Medico] -> [Medico.Medico]
solicitarTransferencia _ _ [] = []
solicitarTransferencia idMed idUBS (x:xs) | idMed == (Medico.id x) = [x {Medico.idUbs = idUBS}] ++ (solicitarTransferencia idMed idUBS xs)
                                          | otherwise = [x] ++ (solicitarTransferencia idMed idUBS xs)

{-

Retorna o id da UBS de um médico.
@param idMed: id do médico
@param medicos: lista de médicos
@return id da UBS onde o médico esta alocado

-}
getUBS :: Int -> [Medico.Medico] -> Int
getUBS _ [] = -1
getUBS idMed (x:xs) | idMed == (Medico.id x) = Medico.idUbs x
                    | otherwise = getUBS idMed xs


{-

Retorna o próximo horário livre do médico e a lista de médicos alterada
@param idMed: id do médico
@param medicos: lista de medicos
@return (horário livre, lista de medicos com o horario livre do medico alterado)

-}
proximoHorarioLivre :: Int -> DateTime -> [Medico.Medico] -> (Maybe DateTime, [Medico.Medico])
proximoHorarioLivre idMed hj medicos = (_proxHorario idMed hj medicos, _medicosAtt idMed hj medicos)

_proxHorario :: Int -> DateTime -> [Medico.Medico] -> Maybe DateTime
_proxHorario _ _ [] = Nothing
_proxHorario idMed hj (x:xs) | idMed == (Medico.id x) = fst (getNextDate (Medico.horarios x) hj)
                             | otherwise = _proxHorario idMed hj xs

_medicosAtt :: Int -> DateTime -> [Medico.Medico] -> [Medico.Medico]
_medicosAtt _ _ [] = []
_medicosAtt idMed hj (x:xs) | idMed == (Medico.id x) = [x {Medico.horarios = snd (getNextDate (Medico.horarios x) hj)}] ++ (_medicosAtt idMed hj xs)
                            | otherwise = [x] ++ (_medicosAtt idMed hj xs)

{-

Verifica se existe médico com o id dado
@param idMed: id do médico
@param medicos: lista dos médicos
@return True se existir, False c.c.

-}
validaIDMedico :: Int -> [Medico.Medico] -> Bool
validaIDMedico _ [] = False
validaIDMedico idMed (x:xs) | idMed == (Medico.id x) = True
                            | otherwise = validaIDMedico idMed xs