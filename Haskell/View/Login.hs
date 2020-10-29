module Haskell.View.Login where

import qualified Haskell.Model.BD as BD
import Haskell.Controller.AutenticacaoController (autentica)
import Data.Char ( toUpper )
import System.IO ( hFlush, stdout )

{-

Interface inicial com o usuário. Apresenta o sistema e oferece as opções de
Login ou Cadastro.

-}

{- 
    Função auxiliar usada para evitar erro na ordem de execução de comandos 
    envolvendo saida e entrada em uma mesma linha.
-}
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

menuInicial :: String -> String
menuInicial m = m ++ "\n"
              ++ " -------------\n" 
              ++ "  (L)ogin     \n"
              ++ "  (C)adastrar \n"
              ++ " -------------\n"

title :: String -> String
title _ = " -------------------------------------------------\n"
        ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) \n"
        ++" -------------------------------------------------\n"

menuLogin :: BD.BD -> IO()
menuLogin dados = do 
    
    id <- prompt ("Informe o id > ")
    senha <- prompt ("Informe a senha > ")
    putStrLn "---------"

    let aut = autentica (BD.logins dados) id senha

    let proxMenu | aut == 0 = menuPaciente
                 | aut == 1 = menuUBS
                 | aut == 2 = menuMedico
                 | otherwise = menuLogin 

    proxMenu dados

menuPaciente :: BD.BD -> IO()
menuPaciente dados = do 
    putStrLn "Paciente"

menuUBS :: BD.BD -> IO()
menuUBS dados = do 
    putStrLn "UBS"

menuMedico :: BD.BD -> IO()
menuMedico dados = do 
    putStrLn "Medico"

menuCadastro :: BD.BD -> IO()
menuCadastro dados = do
    putStrLn "Cadastro"


login :: BD.BD -> IO()
login dados  = do

    putStrLn (title "") 
    putStrLn (menuInicial "")

    op <- prompt ("Opção > ")

    let acao | toUpper (head op) == 'L' = menuLogin
             | toUpper (head op) == 'C' = menuCadastro
             | otherwise = login

    acao dados
