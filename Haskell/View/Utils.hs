module Haskell.View.Utils where

import System.IO ( hFlush, stdout )
import System.Process

import Data.Dates

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
              ++ "  (E)ncerrar \n"
              ++ " -------------\n"

title :: String -> String
title _ = " -------------------------------------------------\n"
        ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) \n"
        ++" -------------------------------------------------\n"

titleCadastro :: String
titleCadastro = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU CADASTRO \n"
              ++" -----------------------------------------------------------------\n"

titleLogin :: String
titleLogin = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU LOGIN \n"
              ++" -----------------------------------------------------------------\n"

titleUBS :: String
titleUBS = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU UBS \n"
              ++" -----------------------------------------------------------------\n"

titlePaciente :: String
titlePaciente = " -----------------------------------------------------------------\n"
              ++"  SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU PACIENTE \n"
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
    sequence [prompt "Nome > ", prompt "CRM > ", prompt "Especialidade > "]

leMedicamento :: IO [String]
leMedicamento = do
    sequence [prompt "Nome > ", prompt "Quantidade > ", prompt "Bula > "]

leHorarioDia :: String -> IO (String, String)
leHorarioDia texto = do
    ini <- prompt ("Horário de início (" ++ texto ++ ") > ")
    fim <- prompt ("Horário de fim (" ++ texto ++ ") > ")
    return (ini, fim)

leHorariosMedico :: IO ([Time], [Time])
leHorariosMedico = do
    putStrLn "Se não atender no dia informe horário de entrada e saida como -1:-1"
    seg <- leHorarioDia "Segunda-feira"
    ter <- leHorarioDia "Terça-feira"
    qua <- leHorarioDia "Quarta-feira"
    qui <- leHorarioDia "Quinta-feira"
    sex <- leHorarioDia "Sexta-feira"
    sab <- leHorarioDia "Sábado"
    dom <- leHorarioDia "Domingo"

    let aux = unzip [seg, ter, qua, qui, sex, sab, dom]
    let begin = map read (fst aux) :: [Time]
    let end = map read (snd aux) :: [Time]

    return (begin, end)

{-

Converte uma string do tipo HH:MM em Time

-}
instance Read Time where 
    readsPrec _ str = do 
    let l = split str ':' "" 
    let hour = read (l !! 0) :: Int
    let minute = read (l !! 1) :: Int   
    [(Time hour minute 0, "")]

formataLista :: Show t => [t] -> String
formataLista [] = ""
formataLista (x:xs) = (show x) ++ "\n" ++ (formataLista xs)

imprime :: Show t => [t] -> IO ()
imprime l = do
    print (formataLista l)
    return ()

medicoMenu :: String
medicoMenu =  " -----------------------------------------------------------------\n"
            ++"   SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU MÉDICO  \n"
            ++" -----------------------------------------------------------------\n"
            ++ "(I)nformar horários\n"
            ++ "(A)cessar Dados\n"
            ++ "(E)mitir\n"
            ++ "(T)ransferência\n"
            ++ "(S)air\n"

medicoAcessarDados :: String
medicoAcessarDados =  "(P)acientes\n"
                    ++ "(E)xames\n"
                    ++ "(A)gendamentos\n"

medicoEmitir :: String
medicoEmitir = "(R)eceita\n"
            ++ "Resultado de (E)xame\n"
            ++ "(L)audo Médico\n"

split :: String -> Char -> String -> [String]
split "" _ "" = []
split "" _ aux = [aux]
split (h : t) sep aux | h == sep && aux == "" = split t sep aux
                  | h == sep = [aux] ++ split t sep ""
                  | otherwise = split t sep (aux ++ [h])