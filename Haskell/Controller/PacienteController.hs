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
  emergencia,
  validaIDPaciente
) where

import Data.List ( intercalate )
import Haskell.Model.Receita
import Haskell.Model.Exame
import Haskell.Model.UBS
import Haskell.Model.Laudo
import Haskell.Model.Paciente
import Haskell.Model.Medico
import qualified Haskell.Model.Consulta as Consulta
import Haskell.Model.Medicamento
import Data.Dates

criaPaciente :: Int -> [String] -> Paciente
criaPaciente idPac infos = read (intercalate ";" ([show (idPac)] ++ infos)) :: Paciente


{-

Buscar as unidades que tem determinada especialidade.
@param especialidade: especialidade buscada
@param medicos: lista de médicos, pois os médicos que guardam as especialidades
@param ubs: lista de ubs
@return lista das ubs que tem médicos com a dada especialidade

-}
buscarUnidades :: String -> [Medico] -> [UBS] -> [UBS]
buscarUnidades esp medicos ubss = [(UBS 1 "" "")]


{-

Cria uma consulta
@param informs: informações da consulta
@param diaC: dia da consulta
@return a consulta criada

-}
requisitarConsulta :: [String] -> DateTime -> Consulta.Consulta
requisitarConsulta informs diaC = (read (intercalate ";" informs)) {Consulta.dia = diaC}

{-

Cria um exame
@param informs: informações da consulta
@param dia: dia da consulta
@return a consulta criada

-}
requisitarExame :: [String] -> DateTime -> Exame
requisitarExame informs dia = (read (intercalate ";" informs)) {dia = dia}

{-

Cria um exame
@param idReceita: id da receita a ser resgatada
@param receitas: lista das receitas
@param medicamentos: lista de medicamentos
@return lista de medicamentos com as quantidades alteradas

-}
requisitarMedicamento :: Int -> [Receita] -> [Medicamento] -> [Medicamento]
requisitarMedicamento idReceita receitas medicamentos = medicamentos

{-

Consultar todos os laudos do paciente
@param idPac: id do paciente
@param laudos: lista dos laudos
@return lista dos laudos do paciente

-}
consultarLaudos :: Int -> [Laudo] -> [Laudo]
consultarLaudos idPac laudos = [(Laudo 1 1 1 "")]

{-

Consultar todos um específico laudos do paciente
@param idPac: id do paciente
@param idLaudo: id do laudo
@param laudos: lista dos laudos
@return laudo procurado

-}
consultarLaudo :: Int -> Int -> [Laudo] -> Laudo
consultarLaudo idPac idLaudo laudos = (Laudo 1 1 1 "")

{-

Consultar todos as receitas de medicamento do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasMed :: Int -> [Receita] -> [Receita]
consultarReceitasMed idPac receitas = [(Receita 1 1 1 1 [(1, "", 1)])]

{-

Consultar uma receita de medicamento do paciente
@param idPac: id do paciente
@param idReceita: id da receita
@param receitas: lista das receitas
@return receita procurada

-}
consultarReceitaMed :: Int -> Int -> [Receita] -> Receita
consultarReceitaMed idPac idReceita receitas = (Receita 1 1 1 1 [(1, "", 1)])

{-

Consultar todos as receitas de exame do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasEx :: Int -> [Exame] -> [Exame]
consultarReceitasEx idPac exames = [(Exame 1 1 1 1 "" (DateTime 2020 10 30 00 00 00) "")]

{-

Consultar uma receita de exame do paciente
@param idPac: id do paciente
@param idReceita: id da receita
@param receitas: lista das receitas
@return receita procurada

-}
consultarReceitaEx :: Int -> Int -> [Exame] -> Exame
consultarReceitaEx idPac idReceita exames = (Exame 1 1 1 1 "" (DateTime 2020 10 30 00 00 00) "")

{-

Recebe um pedido de emergência.
@param idPac: id do paciente
@param endereco: endereço do socorro

-}
emergencia :: Int -> String -> String
emergencia idPac endereco = ""

{-

Verifica se existe paciente com o id dado
@param idPac: id do paciente
@param pacientes: lista dos pacientes
@return True se existir, False c.c.

-}
validaIDPaciente :: Int -> [Paciente] -> Bool
validaIDPaciente idPac pacientes = True