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
import Haskell.Model.Medico
import Haskell.Model.Consulta
import Haskell.Model.Paciente
import Haskell.Model.Medicamento
import Haskell.Model.UBS
import Haskell.Model.DateCycle
import Haskell.Model.Exame
import Haskell.Model.Receita
import Haskell.Model.Laudo

{-

Cria uma UBS
@param idUBS: id da ubs
@param infos: informações da UBS

-}
criaUBS :: Int -> [String] -> UBS
criaUBS idUBS infos = read (intercalate ";" ([show (idUBS)] ++ infos)) :: UBS

{-

Cria um médico
@param idUBS: id da ubs a qual o médico pertence
@param idMed: id do médico
@param informs: informações do médico

-}
cadastraMedico :: Int -> Int -> [String] -> Medico
cadastraMedico idUBS idMed informs = read (intercalate ";" ([show (idUBS), show (idMed)] ++ informs)) :: Medico

{-

Ver todas as consultas agendadas na UBS
@param idUBS: id da ubs
@param consultas: lista das consultas
@return lista das consultas agendadas na UBS

-}
visualizaAgendamentos :: Int -> [Consulta] -> [Consulta]
visualizaAgendamentos idUBS consultas = []

{-

Ver todas as consultas agendadas na UBS
@param idUBS: id da ubs
@param pacientes: lista dos pacientes
@param consultas: lista das consultas
@return lista dos pacientes com consultas agendadas na UBS

-}
visualizaPacientes :: Int -> [Paciente] -> [Consulta] -> [Paciente]
visualizaPacientes idUBS pacientes consultasUBS = []

{-

Ver todos os médicos que trabalham na UBS
@param idUBS: id da ubs
@param medicos: lista dos medicos
@return lista dos medicos da UBS

-}
visualizaMedicos :: Int -> [Medico] -> [Medico]
visualizaMedicos idUBS medicos = []

{-

Ver um médico que trabalha na UBS
@param idMed: id do médico
@param medicos: lista dos medicos
@return médico

-}
visualizaMedico :: Int -> [Medico] -> Medico
visualizaMedico idMed medicos = (Medico 1 "" "" 1 "" empty)

{-

Adiciona um medicamento à ubs
@param idUBS: id da ubs
@param informs: informações do medicamento
@return medicamento criado

-}
adicionaMedicamento :: Int -> [String] -> Medicamento
adicionaMedicamento idUBS informs = read (intercalate ";" ([show (idUBS)] ++ informs)) :: Medicamento

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