module Haskell.Controller.UBSController (
  cadastraMedico,
  visualizaAgendamentos,
  visualizaPacientes,
  visualizaMedicos,
  visualizaMedico,
  adicionaMedicamento,
  adicionaMedicamentoEstoque,
  removerMedicamento,
  visualizaMedicamentos,
  visualizaMedicamento
) where

import Haskell.Model.Medico
import Haskell.Model.Consulta
import Haskell.Model.Paciente
import Haskell.Model.Medicamento

-- descricao: cadastra médico passando as informações
-- retorno: booleano confirmando o cadastro
cadastraMedico :: Int -> Medico -> Bool
cadastraMedico idUBS med = False

visualizaAgendamentos :: Int -> [Consulta]
visualizaAgendamentos idUBS = []

visualizaPacientes :: Int -> [Paciente]
visualizaPacientes idUBS = []

visualizaMedicos :: Int -> [Medico]
visualizaMedicos idUBS = []

visualizaMedico :: Int -> Int -> Medico
visualizaMedico idUBS idMed = (Medico 1 "" "" 1 "" [])

-- descricao: passa as informações do medicamento
adicionaMedicamento :: Int -> Medicamento
adicionaMedicamento idUBS = (Medicamento 1 0 "")

-- descricao: adiciona quantidade ao medicamento
adicionaMedicamentoEstoque :: Int -> Int -> Int -> Medicamento
adicionaMedicamentoEstoque idUBS idMed qtd = (Medicamento 1 0 "")

-- descricao: deduz do estoque
removerMedicamento :: Int -> Int -> Int -> Medicamento
removerMedicamento idUBS idMedicamento qtd = (Medicamento 1 0 "")

visualizaMedicamentos :: Int -> [Medicamento]
visualizaMedicamentos idUBS = []

visualizaMedicamento :: Int -> Medicamento
visualizaMedicamento idUBS = (Medicamento 1 0 "")