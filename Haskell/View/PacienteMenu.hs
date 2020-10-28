module Haskell.View.PacienteMenu where

import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.PacienteController as Controller

title :: String
title = " -----------------------------------------------------------------\n"
      ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU PACIENTE \n"
      ++" -----------------------------------------------------------------\n"

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

{-

Interface do paciente, deve realizar as operações descritas na API

-}

leituraBuscaUnidades :: IO [String]
leituraBuscaUnidades = do
    especialidade <- prompt "Especialidade > "
    print Controller.buscarUnidades especialidade BD.ubs

selecionaRequisitar :: IO String
selecionaRequisitar =  do
    putStrLn "  (C)onsulta"
    putStrLn "  (E)xame"
    putStrLn "  (M)edicamento"

    op <- prompt "Opção > "

    if toUpper (head op) == 'C' then do
        leituraRequisitaConsulta
    else if toUpper (head op) == 'E' then do
        leituraRequisitaExame
    else if toUpper (head op) == 'M' then do
        leituraRequisitaMedicamento
    else return()

leituraRequisitaConsulta :: IO String
leituraRequisitaConsulta = do  
    idPaciente <- prompt "   Meu ID > "
    idMedico <- prompt "   ID do Médico > "
    idUbs <- prompt "   ID da UBS > "
    dia <- prompt "   Dia > "

    -- Validar IDS

    putStrLn Controller.requisitarConsulta (BD.nextId BD) idPaciente idMedico idUbs dia

leituraRequisitaExame :: IO String
leituraRequisitaExame = do  
    idPaciente <- prompt "   Meu ID > "
    idMedico <- prompt "   ID do Médico > "
    idUbs <- prompt "   ID da UBS > "
    tipo <- prompt "    Tipo de Exame > "
    dia <- prompt "   Dia > "

    -- Validar IDS

    putStrLn Controller.requisitarExame (BD.nextId BD) idPaciente idMedico idUbs tipo dia

leituraRequisitaMedicamento :: IO String
leituraRequisitaExame = do  
    idReceita <- prompt "   ID da Receita > "

    -- Validar IDS

    putStrLn Controller.requisitarRemedio idReceita (BD.receitas)

pacienteMenu :: BD.BD -> IO()
pacienteMenu dados = do
    putStrLn title
    putStrLn "(B)uscar unidade por especialidade"
    putStrLn "(R)equisitar"
    putStrLn "(C)onsultar"
    putStrLn "(E)mergência"

    op <- prompt "Opção > "

    if toUpper (head op) == 'B' then do
        leituraBuscaUnidades
    else if toUpper (head op) == 'R' then do
        selecionaRequisitar
    else if toUpper (head op) == 'C' then do
        --
    else if toUpper (head op) == 'E' then do
        --
    else return()
