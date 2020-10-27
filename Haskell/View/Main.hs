--import System.IO ( utf8, hSetEncoding)

import Data.Char ( toUpper )
import System.IO ( hFlush, stdout )

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

menuLogin :: String -> String
menuLogin _ = "Coleta as informações de Login"

menuCadastro :: String -> String
menuCadastro _ = "Coleta as informações de Cadastro"

show_ :: String -> String
show_ str = str 

main :: IO()
main = do 
    --hSetEncoding stdout utf8

    putStrLn (title "") 
    putStrLn (menuInicial "")

    op <- prompt ("Opção > ")

    let acao | toUpper (head op) == 'L' = menuLogin ""
             | toUpper (head op) == 'C' = menuCadastro ""
             | otherwise = (show_ "Opção Inválida")

    putStrLn (acao)
    main
