module Haskell.Controller.PacienteController ( 
  criaPaciente,
  buscarUnidades,
  requisitarConsulta,
  requisitarExame,
  requisitarMedicamento,
  consultarLaudos,
  consultarLaudo,
  consultarReceitasMed,
  consultarReceitaMed,
  consultarReceitasEx,
  consultarReceitaEx,
  emergencia
) where 

import Haskell.Model.Receita
import Haskell.Model.Exame
import Haskell.Model.UBS
import Haskell.Model.Laudo
import Haskell.Model.Paciente

criaPaciente :: Int -> [String] -> Paciente
criaPaciente idPac infos = (Paciente ) --essa linha dá erro, falta preencher as infos do paciente

-- Buscar as unidades que tem determinada especialidade
-- possivelmente mover pro UBS Controller
-- ubsLista = [UBS {indice = 1, idUBS = ""} ]
buscarUnidades :: String -> [UBS] -> [UBS]
buscarUnidades esp ubss = [(UBS 1 "" "")]

-- Recebe a especialidade e a unidade de saúde
requisitarConsulta :: Int -> String -> UBS -> String
requisitarConsulta idPac str ubs = "" 

-- Recebe o código do exame e a unidade de saúde
requisitarExame :: Int -> String -> UBS -> String
requisitarExame idPac str ubs = ""

-- Recebe o nome do medicamento e a unidade de saúde
requisitarMedicamento :: Int -> String -> UBS -> String
requisitarMedicamento idPac str ubs = ""

-- Ver todos os laudos do paciente (a partir do nome do paciente)
-- laudosLista = [Laudos {indice = 1, idPac = "", codigo1 = "", laudo1 = ""} ]
consultarLaudos :: Int -> [Laudo] -> [Laudo]
consultarLaudos idPac laudos = [(Laudo 1 1 "")]

-- Ver um laudo a partir do código (a partir do código do laudo)
-- laudosLista = [Laudos {indice = 1, idPac = "", codigo1 = "", laudo1 = ""} ]
consultarLaudo :: Int -> Int -> [Laudo] -> Laudo
consultarLaudo idPac idLaudo laudos = (Laudo 1 1 "")

-- Ver todas as receitas de medicamento
-- receitasLista = [Receitas {indice = 1, idPac = "", receita1 = ""}]
consultarReceitasMed :: Int -> [Receita] -> [Receita]
consultarReceitasMed idPac receitas = [(Receita 1 1 1 1 [(1, "")])]

-- Ver um receita de medicamento a partir do nome do medicamento
-- receitasLista = [Receitas {indice = 1, idPac = "", receita1 = ""}]
consultarReceitaMed :: Int -> String -> [Receita] -> Receita
consultarReceitaMed idPac medicamento receitas = (Receita 1 1 1 1 [(1, "")])

-- Consultar lista de receitas de exames a partir do idPac
-- examesLista = [Exames {indice = 1, idPac = "", exame = ""}]
consultarReceitasEx :: Int -> [Exame] -> [Exame]
consultarReceitasEx idPac exames = [(Exame 1 1 1 1 "" "" "")]

-- Consultar receita de exame a partir do nome do exame
-- examesLista = [Exames {indice = 1, idPac = "", exame = ""}]
consultarReceitaEx :: Int -> String -> [Exame] -> Exame
consultarReceitaEx idPac exame exames = (Exame 1 1 1 1 "" "" "")

-- Pede atendimento de emergencia passando o endereço
-- Retorna a desc da ambulancia
emergencia :: Int -> String -> String
emergencia idPac endereco = ""
