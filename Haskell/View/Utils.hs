module Haskell.View.Utils where

import System.IO ( hFlush, stdout )
import System.Process

{-

Limpa a tela

-}
clear :: IO ()
clear = do 
    _ <- system "clear"
    return ()

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

titleCadastro :: String
titleCadastro = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU CADASTRO \n"
              ++" -----------------------------------------------------------------\n"

titleUBS :: String
titleUBS = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU UBS \n"
              ++" -----------------------------------------------------------------\n"

opcoesUBS :: IO String
opcoesUBS = do
    putStrLn "(C)adastrar médico"
    putStrLn "(V)isualizar informações"
    putStrLn "(F)armácia"
    putStrLn "(D)ashboard"
    putStrLn "(S)air"
    prompt "Opção > "

opcoesUBSVisualizar :: IO String
opcoesUBSVisualizar = do
    putStrLn "(A)gendamentos"
    putStrLn "(P)aciente"
    putStrLn "(M)édico"
    prompt "Opção > "

opcoesUBSFarmacia :: IO String
opcoesUBSFarmacia = do
    putStrLn "(C)onsultar"
    putStrLn "(N)ovo"
    putStrLn "(A)dicionar"
    putStrLn "(R)emover"
    prompt "Opção > "

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

lePaciente :: IO [String]
lePaciente = do
    sequence [prompt "Nome > ", prompt "CPF > ", prompt "Data de Nascimento > ", prompt "Peso > ", prompt "Altura > ", prompt "Tipo Sanguineo > ", prompt "Endereço > ", prompt "Cardiopata > ", prompt "Diabético > ", prompt "Hipertenso > "]

leUBS :: IO [String]
leUBS = do
    sequence [prompt "Nome > ", prompt "Endereco > "]

leMedico :: IO [String]
leMedico = do
    sequence [prompt "Nome > ", prompt "CRM > ", prompt "Especialidade > ", prompt "Horário (formato: Seg-HH:MM-HH:MM;Ter-HH:MM-HH:MM)> "]

leMedicamento :: IO [String]
leMedicamento = do
    sequence [prompt "Nome > ", prompt "Quantidade > ", prompt "Bula > "]