module Haskell.Controller.UBSController where

import Data.List ( intercalate )
import Data.Dates
import qualified Haskell.Model.Medico as Medico
import qualified Haskell.Model.Consulta as Consulta
import qualified Haskell.Model.Paciente as Paciente
import qualified Haskell.Model.Medicamento as Medicamento
import qualified Haskell.Model.UBS as UBS
import qualified Haskell.Model.Exame as Exame
import qualified Haskell.Model.Receita as Receita
import qualified Haskell.Model.Laudo as Laudo
import Haskell.Model.DateCycle

{-

Cria uma UBS
@param idUBS: id da ubs
@param infos: informações da UBS

-}
criaUBS :: Int -> [String] -> UBS.UBS
criaUBS idUBS infos = read (intercalate ";" ([show (idUBS)] ++ infos))

{-

Cria um médico
@param idUBS: id da ubs a qual o médico pertence
@param idMed: id do médico
@param infos: informações do médico

-}
cadastraMedico :: Int -> Int -> [String] -> Medico.Medico
cadastraMedico idUBS idMed informs = read (intercalate ";" ([show (idUBS), show (idMed)] ++ informs)) :: Medico.Medico

{-

Ver todas as consultas agendadas na UBS
@param idUBS: id da ubs
@param consultas: lista das consultas
@return lista das consultas agendadas na UBS

-}
visualizaAgendamentos :: Int -> [Consulta.Consulta] -> DateTime -> [Consulta.Consulta]
visualizaAgendamentos _ [] _ = []
visualizaAgendamentos idUBS (x:xs) hj| idUBS == (Consulta.idUBS x) && hj <= (Consulta.dia x) = [x] ++ (visualizaAgendamentos idUBS xs hj)
                                     | otherwise = visualizaAgendamentos idUBS xs hj

{-

Ver todas as consultas agendadas na UBS
@param idUBS: id da ubs
@param consultas: lista das consultas
@return lista das consultas agendadas na UBS

-}
visualizaAgendamentosTodos :: Int -> [Consulta.Consulta] -> [Consulta.Consulta]
visualizaAgendamentosTodos _ [] = []
visualizaAgendamentosTodos idUBS (x:xs) | idUBS == (Consulta.idUBS x) = [x] ++ (visualizaAgendamentosTodos idUBS xs)
                                        | otherwise = visualizaAgendamentosTodos idUBS xs

{-

Ver todos os pacientes na UBS
@param idUBS: id da ubs
@param pacientes: lista dos pacientes
@param consultas: lista das consultas
@return lista dos pacientes com consultas agendadas na UBS

-}
visualizaPacientes :: Int -> [Paciente.Paciente] -> [Consulta.Consulta] -> [Paciente.Paciente]
visualizaPacientes _ [] _ = []
visualizaPacientes _ _ [] = []
visualizaPacientes idUBS (x:xs) consulta | (_verificaConsultaPaciente idUBS x consulta) = [x] ++ (visualizaPacientes idUBS xs consulta)
                                         | otherwise = visualizaPacientes idUBS xs consulta

_verificaConsultaPaciente :: Int -> Paciente.Paciente -> [Consulta.Consulta] -> Bool
_verificaConsultaPaciente _ _ [] = False
_verificaConsultaPaciente idUBS paciente (x:xs) | (idUBS == Consulta.idUBS x) && (Paciente.id paciente) == (Consulta.idPaciente x) = True
                                                | otherwise = _verificaConsultaPaciente idUBS paciente xs

{-

Ver todos os médicos que trabalham na UBS
@param idUBS: id da ubs
@param medicos: lista dos medicos
@return lista dos medicos da UBS

-}
visualizaMedicos :: Int -> [Medico.Medico] -> [Medico.Medico]
visualizaMedicos _ [] = []
visualizaMedicos idUBS (x:xs) | idUBS == (Medico.idUbs x) = [x] ++ (visualizaMedicos idUBS xs)
                              | otherwise = visualizaMedicos idUBS xs

{-

Ver um médico que trabalha na UBS
@param idMed: id do médico
@param medicos: lista dos medicos
@return médico

-}
visualizaMedico :: Int -> Int -> [Medico.Medico] -> Maybe Medico.Medico
visualizaMedico _ _ [] = Nothing
visualizaMedico idUBS idMed (x:xs) | idMed == (Medico.id x) && idUBS == (Medico.idUbs x) = Just x
                                   | otherwise = visualizaMedico idUBS idMed xs

{-

Adiciona um medicamento à ubs
@param idUBS: id da ubs
@param informs: informações do medicamento
@return medicamento criado

-}
adicionaMedicamento :: Int -> [String] -> Medicamento.Medicamento
adicionaMedicamento idUBS informs = read (intercalate ";" ([show (idUBS)] ++ informs))

{-

Adiciona uma quantidade no estoque de um medicamento
@param idUBS: id da ubs
@param idMed: id do medicamento
@param qtd: quantidade a adicionar
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos com o medicamento alterado

-}
adicionaMedicamentoEstoque :: Int -> Int -> Int -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
adicionaMedicamentoEstoque _ _ _ [] = []
adicionaMedicamentoEstoque idUBS idMed qtd (x:xs) | (idUBS == Medicamento.idUBS x) && (idMed == Medicamento.id x) = [x {Medicamento.qtdEstoque = (Medicamento.qtdEstoque x) + qtd}] ++ (adicionaMedicamentoEstoque idUBS idMed qtd xs)
                                                  | otherwise = adicionaMedicamentoEstoque idUBS idMed qtd xs

{-

Remove uma quantidade no estoque de um medicamento
@param idUBS: id da ubs
@param idMed: id do medicamento
@param qtd: quantidade a retirar
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos com o medicamento alterado

-}
removerMedicamento :: Int -> Int -> Int -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
removerMedicamento _ _ _ [] = []
removerMedicamento idUBS idMed qtd (x:xs) | (idUBS == Medicamento.idUBS x) && (idMed == Medicamento.id x) = [x {Medicamento.qtdEstoque = (Medicamento.qtdEstoque x) - qtd}] ++ (adicionaMedicamentoEstoque idUBS idMed qtd xs)
                                                  | otherwise = removerMedicamento idUBS idMed qtd xs

{-

Ver todos os medicamentos disponíveis na UBS
@param idUBS: id da ubs
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos da UBS

-}
visualizaMedicamentos :: Int -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
visualizaMedicamentos _ [] = []
visualizaMedicamentos idUBS (x:xs) | idUBS == Medicamento.idUBS x = [x] ++ visualizaMedicamentos idUBS xs
                                   | otherwise = visualizaMedicamentos idUBS xs

{-

Ver um medicamento disponível na UBS
@param idMed: id do medicamento
@param medicamentos: lista dos medicamentos
@return medicamento

-}
visualizaMedicamento :: Int-> Int -> [Medicamento.Medicamento] -> Maybe Medicamento.Medicamento
visualizaMedicamento _ _ [] = Nothing
visualizaMedicamento idUBS idMed (x:xs) | (idUBS == Medicamento.idUBS x) && (idMed == Medicamento.id x) = Just x
                                        | otherwise = visualizaMedicamento idUBS idMed xs

{-

Verifica se existe medicamento com o id dado
@param idMed: id do medicamento
@param medicamentos: lista dos medicamentos
@return True se existir, False c.c.

-}
validaIDMedicamento :: Int -> [Medicamento.Medicamento] -> Bool
validaIDMedicamento _ [] = False
validaIDMedicamento idMed (x:xs) | idMed == (Medicamento.id x) = True
                                 | otherwise = validaIDMedicamento idMed xs

{-

Verifica se existe exame com o id dado
@param idExame: id do exame
@param exames: lista dos exames
@return True se existir, False c.c.

-}
validaIDExame :: Int -> [Exame.Exame] -> Bool
validaIDExame _ [] = False
validaIDExame idExame (x:xs) | idExame == (Exame.id x) = True
                             | otherwise = validaIDExame idExame xs

{-

Verifica se existe UBS com o id dado
@param idUBS: id do UBS
@param UBSs: lista dos UBSs
@return True se existir, False c.c.

-}
validaIDUBS :: Int -> [UBS.UBS] -> Bool
validaIDUBS _ [] = False
validaIDUBS idUBS (x:xs) | idUBS == (UBS.id x) = True
                         | otherwise = validaIDUBS idUBS xs

{-

Verifica se existe Receita com o id dado
@param idReceita: id do Receita
@param receitas: lista dos Receitas
@return True se existir, False c.c.

-}
validaIDReceita :: Int -> [Receita.Receita] -> Bool
validaIDReceita _ [] = False
validaIDReceita idReceita (x:xs) | idReceita == (Receita.id x) = True
                                 | otherwise = validaIDReceita idReceita xs

{-

Lista as consultas do dia atual
@param hoje: data do dia atual
@param consultas: lista de Consultas
@return consultas do dia.

-}
getConsultasDoDia :: DateTime -> [Consulta.Consulta] -> [String]
getConsultasDoDia _ [] = []
getConsultasDoDia hoje (x:xs) | hoje === (Consulta.dia x) = [Consulta.formataConsulta x] ++ (getConsultasDoDia hoje xs)
                              | otherwise = getConsultasDoDia hoje xs

{-

Lista os médicos disponíveis 
@param consultas: lista de Consultas
@param medicos: lista de Médicos
@return -1 se o médico não está de plantão, 0 se o médico está de plantão sem consulta,
        1 se o médico está em consulta

-}
getStatusMedicos :: DateTime -> [Consulta.Consulta] -> [Medico.Medico] -> [(Medico.Medico, Int)]
getStatusMedicos _ _ [] = []
getStatusMedicos hj consultas (x:xs) | isEmpty (Medico.horarios x) = [(x, 2)] ++ (getStatusMedicos hj consultas xs)
                                     | inicioPlantao == (Time (-1) (-1) 0) = [(x, -1)] ++ (getStatusMedicos hj consultas xs)
                                     | (dateTimeToTime hj) <= inicioPlantao || (dateTimeToTime hj) >= fimPlantao = [(x,-1)] ++ (getStatusMedicos hj consultas xs)
                                     | otherwise = [(x, statusMedico x consultas hj)] ++ (getStatusMedicos hj consultas xs)
                                     where
                                      weekd = (weekdayNumber (dateWeekDay hj)) - 1
                                      inicioPlantao = startD weekd (Medico.horarios x)
                                      fimPlantao = endD weekd (Medico.horarios x)

statusMedico :: Medico.Medico -> [Consulta.Consulta] -> DateTime -> Int
statusMedico _ [] _ = 0 -- medico está de plantão e sem consulta
statusMedico m (x:xs) hj | s <= hj && e >= hj = 1 -- medico está em consulta
                         | otherwise = statusMedico m xs hj
                         where
                          s = (Consulta.dia x)
                          e = correctDate (addTime s (Time 0 (timeSc (Medico.horarios m)) 0)) -- termino da consulta

{-

Formata os medicamentos para a dashboard
@param medicamentos: Lista com os 5 medicamentos com pouco estoque
@return retorna todos os medicamentos no formato Nome - Estoque 

-}
formataMedicamentosDashboard :: [Medicamento.Medicamento] -> String
formataMedicamentosDashboard [] = ""
formataMedicamentosDashboard (x:xs) = (Medicamento.nome x) ++ " - " ++ (show (Medicamento.qtdEstoque x)) ++ "\n" ++ (formataMedicamentosDashboard xs)

{-

Formata os médicos para a dashboard
@param medicos: Lista com os médicos a serem mostrados na dashboard
@return retorna todos os médicos no formato ID - Nome - Status

-}
formataMedicosDashboard :: [(Medico.Medico, Int)] -> String
formataMedicosDashboard [] = ""
formataMedicosDashboard ((a, b):xs) = (show (Medico.id a)) ++ " " ++ (Medico.nome a) ++ " - " ++ (formataStatus b) ++ "\n" ++ (formataMedicosDashboard xs)

formataStatus :: Int -> String
formataStatus n | n == -1 = "Não está em plantão"
                | n == 0 = "Está de plantão e sem consulta"
                | n == 1 = "Está de plantão e em consulta"
                | n == 2 = "Médico ainda não informou horários"
                | otherwise = (show n)

validaIDLaudo :: Int -> [Laudo.Laudo] -> Bool
validaIDLaudo _ [] = False
validaIDLaudo idLaudo (x:xs) | idLaudo == (Laudo.id x) = True
                             | otherwise = validaIDLaudo idLaudo xs

validaReceita :: [(Int, String, Int)] -> [Medicamento.Medicamento] -> Bool
validaReceita [] _ = True
validaReceita ((id,_,_):xs) m = (validaIDMedicamento id m) && (validaReceita xs m)
