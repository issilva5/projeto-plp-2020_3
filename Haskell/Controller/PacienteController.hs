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

{-

Cria um paciente.
@param idPac: o id do paciente
@param infos: as informações do paciente
@return o paciente criado.

-}
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
buscarUnidades _ [] _ = []
buscarUnidades esp (x:xs) ubss | esp == (Medico.especialidade x) = (_buscarUnidades x ubss) ++ (buscarUnidades esp xs ubss)
                              | otherwise = buscarUnidades esp xs ubss

_buscarUnidades :: Medico -> [UBS] -> [UBS]
_buscarUnidades _ [] = []
_buscarUnidades medico (x:xs) | (Medico.idUbs medico) == (UBS.id x) = [x] ++ _buscarUnidades medico xs
                              | otherwise = _buscarUnidades medico xs

{-

Cria uma consulta.
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

Busca os remédios e atualiza o estoque
@param idReceita: id da receita a ser resgatada
@param receitas: lista das receitas
@param medicamentos: lista de medicamentos
@return lista de medicamentos com as quantidades alteradas

-}
requisitarMedicamento :: Int -> [Receita] -> [Medicamento] -> [Medicamento]
requisitarMedicamento _ [] _ = []
requisitarMedicamento idReceita (x:xs) medicamentos = [(_requisitarMedicamento (Receita.remedios x) medicamentos)] ++ (requisitarMedicamento idReceita xs medicamentos)

_requisitarMedicamento :: [(Int, String, Int)] -> [Medicamento] -> [Medicamento]
_requisitarMedicamento [] _ = []
_requisitarMedicamento (x:xs) medicamentos = [(_atualizarEstoque x medicamentos)] ++ (_requisitarMedicamento xs medicamentos)

_atualizarEstoque :: (Int, String, Int) -> [Medicamento] -> [Medicamento]
_atualizarEstoque _ [] = []
_atualizarEstoque remedio (x:xs) | (remedio !! 0) == (Medicamento.id x) = [x {qtdEstoque = x.qtdEstoque - (remedio !! 2)}] ++ _atualizarEstoque remedio xs
                                | otherwise = _atualizarEstoque remedio xs

{-

Consultar todos os laudos do paciente
@param idPac: id do paciente
@param laudos: lista dos laudos
@return lista dos laudos do paciente

-}
consultarLaudos :: Int -> [Laudo] -> [Exame] -> [Laudo]
consultarLaudos _ _ [] = []
consultarLaudos idPac laudos (x:xs) | idPac == (Exame.idPaciente x) = (_consultarLaudos x laudos) ++ (consultarLaudos idPac laudos xs)
                                    | otherwise = consultarLaudos idPac laudos xs

_consultarLaudos :: Exame -> [Laudo]
_consultarLaudos _ [] = []
_consultarLaudos exame (x:xs) | (Exame.id exame) == (Laudo.idExame x) = [x] ++ (_consultarLaudos exame xs)
                              | otherwise = _consultarLaudos exame xs


{-

Consultar um específico laudo de um paciente
@param idPac: id do paciente
@param idLaudo: id do laudo
@param laudos: lista dos laudos
@return laudo procurado

-}
consultarLaudo :: Int -> Int -> [Laudo] -> Maybe Laudo
consultarLaudo _ _ [] = Nothing
consultarLaudo idPac idLaudo (x:xs) | idLaudo == (Laudo.id x) = x
                                    | otherwise = consultarLaudo idPac idLaudo xs


{-

Consultar todos as receitas médicas do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasMed :: Int -> [Receita] -> [Receita]
consultarReceitasMed _ [] = []
consultarReceitasMed idPac (x:xs) | idPac == (Receita.idPaciente x) = [x] ++ (consultarReceitasMed idPac xs)
                                  | otherwise = consultarReceitasMed idPac xs


{-

Consultar uma receita de medicamento do paciente
@param idPac: id do paciente
@param idReceita: id da receita
@param receitas: lista das receitas
@return receita procurada

-}
consultarReceitaMed :: Int -> Int -> [Receita] -> Maybe Receita
consultarReceitaMed _ _ [] = Nothing
consultarReceitaMed idPac idReceita (x:xs) | idReceita == (Receita.id x) = x
                                          | otherwise = consultarReceitaMed idPac idReceita xs


{-

Consultar todos as receitas de exame do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasEx :: Int -> [Exame] -> [Exame]
consultarReceitasEx _ [] = []
consultarReceitasEx idPac (x:xs) | idPac == (Exame.idPaciente x) = [x] ++ (consultarReceitasEx idPac xs)
                                | otherwise = consultarReceitasEx idPac xs


{-

Consultar uma receita de exame do paciente
@param idPac: id do paciente
@param idExame: id do exame
@param receitas: lista das receitas
@return receita procurada

-}
consultarReceitaEx :: Int -> Int -> [Exame] -> Maybe Exame
consultarReceitaEx _ _ [] = Nothing
consultarReceitaEx idPac idExame (x:xs) | idExame == (Exame.id x) = x
                                        | otherwise = consultarReceitaEx idPac idExame xs

{-

Recebe um pedido de emergência.
@param idPac: id do paciente
@param endereco: endereço do socorro

-}
emergencia :: Int -> String -> String
emergencia idPac endereco = "🚑 Ambulância para " ++ endereco ++ " está a caminho! 🩺"


{-

Verifica se existe paciente com o id dado
@param idPac: id do paciente
@param pacientes: lista dos pacientes
@return True se existir, False c.c.

-}
validaIDPaciente :: Int -> [Paciente] -> Bool
validaIDPaciente _ [] = False
validaIDPaciente idPac (x:xs) | idPac == (Paciente.id x) = True 
                              | otherwise = validaIDPaciente idPac xs