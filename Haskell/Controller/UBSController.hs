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
  visualizaMedicamento
) where

import Haskell.Model.Medico
import Haskell.Model.Consulta
import Haskell.Model.Paciente
import Haskell.Model.Medicamento
import Haskell.Model.UBS

criaUBS :: Int -> [String] -> UBS
criaUBS idUBS infos = (UBS 1 "" "")

-- descricao: cadastra médico passando as informações
-- retorno: booleano confirmando o cadastro
cadastraMedico :: Int -> Int -> [String] -> Medico
cadastraMedico idUBS idMed informs = Medico 1 "" "" 1 "" [""]

visualizaAgendamentos :: Int -> [Consulta] -> [Consulta]
visualizaAgendamentos idUBS consultas = []

-- visualizaPacientes com consulta marcada na UBS
-- recebe: pacientes, consultas da ubs
visualizaPacientes :: [Paciente] -> [Consulta] -> [Paciente]
visualizaPacientes pacientes consultasUBS = []

visualizaMedicos :: Int -> [Medico] -> [Medico]
visualizaMedicos idUBS medicos = []

visualizaMedico :: Int -> Int -> [Medico] -> Medico
visualizaMedico idUBS idMed medicos = (Medico 1 "" "" 1 "" [])

-- descricao: passa as informações do medicamento
adicionaMedicamento :: Int -> Int -> [String] -> Medicamento
adicionaMedicamento idUBS idMedic informs = (Medicamento 1 1 "" 0 "")

-- descricao: adiciona quantidade ao medicamento
-- busca pelo medicamento requisitado altera sua quantidade e retorna a lista dos medicamentos
adicionaMedicamentoEstoque :: Int -> Int -> Int -> [Medicamento] -> [Medicamento]
adicionaMedicamentoEstoque idUBS idMed qtd medicamentos = []

-- descricao: deduz do estoque
-- busca pelo medicamento requisitado altera sua quantidade e retorna a lista dos medicamento
removerMedicamento :: Int -> Int -> Int -> [Medicamento] -> [Medicamento]
removerMedicamento idUBS idMedicamento qtd medicamentos = []

visualizaMedicamentos :: Int -> [Medicamento] -> [Medicamento]
visualizaMedicamentos idUBS medicamentos = []

visualizaMedicamento :: Int -> [Medicamento] -> Medicamento
visualizaMedicamento idMedic medicamentos = (Medicamento 1 1 "" 0 "")
