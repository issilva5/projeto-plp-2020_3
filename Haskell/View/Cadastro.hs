module Haskell.View.Cadastro (
    cadastra
) where

import qualified Haskell.Model.BD as BD
import qualified Haskell.View.Login as Login
import qualified Haskell.Controller.PacienteController as PC

import System.IO ( hFlush, stdout )
import Data.Char ( toUpper )

title :: String
title = " -----------------------------------------------------------------\n"
      ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU CADASTRO \n"
      ++" -----------------------------------------------------------------\n"

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

{-

Interface de cadastro de Paciente e UBS, lê as informações, os cria e depois retorna para o login

-}

lePaciente :: IO [String]
lePaciente = do
    sequence [prompt "Nome > ", prompt "CPF > ", prompt "Data de Nascimento > ", prompt "Peso > ", prompt "Altura > ", prompt "Tipo Sanguineo > ", prompt "Endereço > ", prompt "Cardiopata > ", prompt "Diabético > ", prompt "Hipertenso > "]

leUBS :: IO [String]
leUBS = do
    sequence [prompt "Nome > ", prompt "Endereco > "]

cadastra :: BD.BD -> IO()
cadastra dados = do
    putStrLn title
    putStrLn "(P)aciente"
    putStrLn "(U)BS"
    op <- prompt "Opção > "
    
    putStrLn ""

    if toUpper (head op) == 'P' then do
        Login.login dados {BD.pacientes = (BD.pacientes dados) ++ [PC.criaPaciente lePaciente]}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        print dadosU
    else return ()