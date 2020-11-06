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
import qualified Haskell.Model.Receita as Receita
import qualified Haskell.Model.Exame as Exame
import qualified Haskell.Model.UBS as UBS
import qualified Haskell.Model.Laudo as Laudo
import qualified Haskell.Model.Paciente as Paciente
import qualified Haskell.Model.Medico as Medico
import qualified Haskell.Model.Consulta as Consulta
import qualified Haskell.Model.Medicamento as Medicamento
import Data.Dates

{-

Cria um paciente.
@param idPac: o id do paciente
@param infos: as informações do paciente
@return o paciente criado.

-}
criaPaciente :: Int -> [String] -> Paciente.Paciente
criaPaciente idPac infos = read (intercalate ";" ([show (idPac)] ++ infos)) :: Paciente.Paciente


{-

Buscar as unidades que tem determinada especialidade.
@param especialidade: especialidade buscada
@param medicos: lista de médicos, pois os médicos que guardam as especialidades
@param ubs: lista de ubs
@return lista das ubs que tem médicos com a dada especialidade

-}
buscarUnidades :: String -> [Medico.Medico] -> [UBS.UBS] -> [UBS.UBS]
buscarUnidades _ [] _ = []
buscarUnidades esp (x:xs) ubss | esp == (Medico.especialidade x) = (_buscarUnidades x ubss) ++ (buscarUnidades esp xs ubss)
                               | otherwise = buscarUnidades esp xs ubss

_buscarUnidades :: Medico.Medico -> [UBS.UBS] -> [UBS.UBS]
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
requisitarExame :: [String] -> DateTime -> Exame.Exame
requisitarExame informs dia = (read (intercalate ";" informs)) {Exame.dia = dia}


{-

Busca os remédios e atualiza o estoque
@param idReceita: id da receita a ser resgatada
@param receitas: lista das receitas
@param medicamentos: lista de medicamentos
@return lista de medicamentos com as quantidades alteradas

-}
requisitarMedicamento :: Int -> [Receita.Receita] -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
requisitarMedicamento _ [] medicamentos = medicamentos
requisitarMedicamento idReceita (x:xs) medicamentos | idReceita == (Receita.id x) = _requisitarMedicamento (Receita.remedios x) medicamentos
                                                    | otherwise = requisitarMedicamento idReceita xs medicamentos

_requisitarMedicamento :: [(Int, String, Int)] -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
_requisitarMedicamento [] medicamentos = medicamentos
_requisitarMedicamento (x:xs) medicamentos = _requisitarMedicamento xs (_atualizarEstoque x medicamentos)

_atualizarEstoque :: (Int, String, Int) -> [Medicamento.Medicamento] -> [Medicamento.Medicamento]
_atualizarEstoque _ [] = []
_atualizarEstoque (id, bula, qtd) (x:xs) | id == (Medicamento.id x) = [x {Medicamento.qtdEstoque = (Medicamento.qtdEstoque x) - qtd}] ++ (_atualizarEstoque (id, bula, qtd) xs)
                                         | otherwise = [x] ++ (_atualizarEstoque (id, bula, qtd) xs)

{-

Consultar todos os laudos do paciente
@param idPac: id do paciente
@param laudos: lista dos laudos
@return lista dos laudos do paciente

-}
consultarLaudos :: Int -> [Laudo.Laudo] -> [Exame.Exame] -> [Laudo.Laudo]
consultarLaudos _ _ [] = []
consultarLaudos idPac laudos (x:xs) | idPac == (Exame.idPaciente x) = (_consultarLaudos x laudos) ++ (consultarLaudos idPac laudos xs)
                                    | otherwise = consultarLaudos idPac laudos xs

_consultarLaudos :: Exame.Exame -> [Laudo.Laudo] -> [Laudo.Laudo]
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
consultarLaudo :: Int -> [Laudo.Laudo] -> Maybe Laudo.Laudo
consultarLaudo _ [] = Nothing
consultarLaudo idLaudo (x:xs) | idLaudo == (Laudo.id x) = Just x
                              | otherwise = consultarLaudo idLaudo xs


{-

Consultar todos as receitas médicas do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasMed :: Int -> [Receita.Receita] -> [Receita.Receita]
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
consultarReceitaMed :: Int -> [Receita.Receita] -> Maybe Receita.Receita
consultarReceitaMed _ [] = Nothing
consultarReceitaMed idReceita (x:xs) | idReceita == (Receita.id x) = Just x
                                     | otherwise = consultarReceitaMed idReceita xs


{-

Consultar todos as receitas de exame do paciente
@param idPac: id do paciente
@param receitas: lista das receitas
@return lista das receitas do paciente

-}
consultarReceitasEx :: Int -> [Exame.Exame] -> [Exame.Exame]
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
consultarReceitaEx :: Int -> [Exame.Exame] -> Maybe Exame.Exame
consultarReceitaEx _ [] = Nothing
consultarReceitaEx idExame (x:xs) | idExame == (Exame.id x) = Just x
                                        | otherwise = consultarReceitaEx idExame xs

{-

Recebe um pedido de emergência.
@param idPac: id do paciente
@param endereco: endereço do socorro

-}
emergencia :: String -> String
emergencia endereco = "Ambulância para " ++ endereco ++ " está a caminho!"


{-

Verifica se existe paciente com o id dado
@param idPac: id do paciente
@param pacientes: lista dos pacientes
@return True se existir, False c.c.

-}
validaIDPaciente :: Int -> [Paciente.Paciente] -> Bool
validaIDPaciente _ [] = False
validaIDPaciente idPac (x:xs) | idPac == (Paciente.id x) = True
                              | otherwise = validaIDPaciente idPac xs