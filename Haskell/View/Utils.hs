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

titleCadastro :: String
titleCadastro = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU CADASTRO \n"
              ++" -----------------------------------------------------------------\n"

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
    sequence [prompt "Nome > ", prompt "CRM > ", prompt "ID da UBS > ", prompt "Especialidade > ", prompt "Horarios > "]

medicoMenu :: String
medicoMenu =  " -----------------------------------------------------------------\n"
            ++"   SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU MÉDICO  \n"
            ++" -----------------------------------------------------------------\n"
            ++ "(I)nformar horários\n"
            ++ "(A)cessar Dados\n"
            ++ "(E)mitir\n"
            ++ "(T)ransferência\n"

medicoAcessarDados :: String
medicoAcessarDados =   "Qual dado deseja acessar?"
                    ++ "(P)acientes"
                    ++ "(E)xames"
                    ++ "(A)gendamentos"

medicoEmitir :: String
medicoEmitir = "Qual dado deseja acessar?"
            ++ "(R)eceita"
            ++ "(S)olicitação de Exame"
            ++ "(L)audo Médico"