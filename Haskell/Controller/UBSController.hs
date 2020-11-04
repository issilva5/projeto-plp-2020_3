module Haskell.Controller.UBSController (
  criaUBS,
  cadastraMedico,
  visualizaAgendamentos,
  visualizaPacientes,
  visualizaMedicos,
  visualizaMedico,
  adicionaMedicamento,
  adicionaMedicamentoEstoque,
  removerMedicamento,
  visualizaMedicamentos,
  visualizaMedicamento,
  validaIDMedicamento,
  validaIDExame,
  validaIDUBS,
  validaIDReceita,
  validaIDLaudo
) where

import Data.List ( intercalate )
import Haskell.Model.DateCycle
import qualified Haskell.Model.Medico as Medico
import qualified Haskell.Model.Consulta as Consulta
import qualified Haskell.Model.Paciente as Paciente
import qualified Haskell.Model.Medicamento as Medicamento
import qualified Haskell.Model.UBS as Ubs
import qualified Haskell.Model.Exame as Exame
import qualified Haskell.Model.Receita as Receita
import qualified Haskell.Model.Laudo as Laudo



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
cadastraMedico idUBS idMed infos = read (intercalate ";" ([show (idUBS), show (idMed)] ++ infos))

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

Ver todos os pacientes na UBS
@param idUBS: id da ubs
@param pacientes: lista dos pacientes
@param consultas: lista das consultas
@return lista dos pacientes com consultas agendadas na UBS

-}
visualizaPacientes :: Int -> [Paciente.Paciente] -> [Consulta.Consulta] -> [Paciente.Paciente]
visualizaPacientes _ [] _ = []
visualizaPacientes _ _ [] = []
visualizaPacientes idUBS (x:xs) consulta | (_verificaConsultaPaciente idUBS x consulta) = [x] ++ (visualizaPacientes idUbs xs consulta)
                                         | otherwise = visualizaPacientes idUbs xs consulta

_verificaConsultaPaciente :: Int -> Paciente.Paciente -> [Consulta.Consulta] -> Bool
_verificaConsultaPaciente _ _ [] = False
_verificaConsultaPaciente idUBS paciente (x:xs) | (idUBS == Consulta.idUBS x) && (Paciente.id paciente) == (Consulta.idPaciente x) = True
                                                | otherwise = _verificaConsultaPaciente paciente xs

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
visualizaMedico idUBS idMed (x:xs) | idMed == (Medico.id x) && idUBS == (Medico.idUbs x) = [x] ++ (visualizaMedico idUBS idMed xs)
                                   | otherwise = visualizaMedico idUBS idMed xs

{-

Adiciona um medicamento à ubs
@param idUBS: id da ubs
@param informs: informações do medicamento
@return medicamento criado

-}
adicionaMedicamento :: Int -> [String] -> Medicamento
adicionaMedicamento idUBS informs = read (intercalate ";" ([show (idUBS)] ++ informs))

{-

Adiciona uma quantidade no estoque de um medicamento
@param idUBS: id da ubs
@param idMed: id do medicamento
@param qtd: quantidade a adicionar
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos com o medicamento alterado

-}
adicionaMedicamentoEstoque :: Int -> Int -> Int -> [Medicamento] -> [Medicamento]
adicionaMedicamentoEstoque idUBS idMed qtd medicamentos = []

{-

Remove uma quantidade no estoque de um medicamento
@param idUBS: id da ubs
@param idMed: id do medicamento
@param qtd: quantidade a adicionar
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos com o medicamento alterado

-}
removerMedicamento :: Int -> Int -> Int -> [Medicamento] -> [Medicamento]
removerMedicamento idUBS idMedicamento qtd medicamentos = []

{-

Ver todos os medicamentos disponíveis na UBS
@param idUBS: id da ubs
@param medicamentos: lista dos medicamentos
@return lista dos medicamentos da UBS

-}
visualizaMedicamentos :: Int -> [Medicamento] -> [Medicamento]
visualizaMedicamentos idUBS medicamentos = []

{-

Ver um medicamentos disponível na UBS
@param idMedic: id do medicamento
@param medicamentos: lista dos medicamentos
@return medicamento

-}
visualizaMedicamento :: Int -> [Medicamento] -> Medicamento
visualizaMedicamento idMedic medicamentos = (Medicamento 1 1 "" 0 "")

{-

Verifica se existe medicamento com o id dado
@param idMedic: id do medicamento
@param medicamentos: lista dos medicamentos
@return True se existir, False c.c.

-}
validaIDMedicamento :: Int -> [Medicamento] -> Bool
validaIDMedicamento idMedic medicamentos = True

{-

Verifica se existe exame com o id dado
@param idExame: id do exame
@param exames: lista dos exames
@return True se existir, False c.c.

-}
validaIDExame :: Int -> [Exame] -> Bool
validaIDExame idExame exames = True

{-

Verifica se existe UBS com o id dado
@param idUBS: id do UBS
@param UBSs: lista dos UBSs
@return True se existir, False c.c.

-}
validaIDUBS :: Int -> [UBS] -> Bool
validaIDUBS idUBS ubs = True

{-

Verifica se existe Receita com o id dado
@param idReceita: id do Receita
@param receitas: lista dos Receitas
@return True se existir, False c.c.

-}
validaIDReceita :: Int -> [Receita] -> Bool
validaIDReceita idReceita receitas = True

{-

Verifica se existe Laudo com o id dado
@param idLaudo: id do Laudo
@param laudos: lista dos Laudos
@return True se existir, False c.c.

-}
validaIDLaudo :: Int -> [Laudo] -> Bool
validaIDLaudo idReceita laudos = True