module Haskell.View.Utils where

import System.IO ( hFlush, stdout )
import System.Process

import Data.Dates

import Data.Char ( toUpper )

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
    sequence [prompt "Nome > ", prompt "CPF > ", prompt "Data de Nascimento > ", prompt "Peso > ", prompt "Altura > ", prompt "Tipo Sanguineo > ", prompt "Endereço > ", prompt "Cardiopata (S ou N) > ", prompt "Diabético (S ou N) > ", prompt "Hipertenso (S ou N) > "]

leUBS :: IO ([String])
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

instance Read DateTime where
    readsPrec _ str = do 
    let dt = split str ' ' ""
    let d = split (dt !! 0) '/' ""
    let t = if (length dt) == 2 then split (dt !! 1) ':' "" else []
    let day = read (d !! 0) :: Int
    let month = read (d !! 1) :: Int
    let year = read (d !! 2) :: Int
    let hour = if (length t) >= 1 then read (t !! 0) :: Int else 0
    let minute = if (length t) >= 2 then read (t !! 1) :: Int else 0
    let second = if (length t) == 3 then read (t !! 2) :: Int else 0
    [(DateTime year month day hour minute second, "")]

formataLista :: Show t => [t] -> String
formataLista [] = ""
formataLista (x:xs) = (show x) ++ "\n" ++ (formataLista xs)

imprime :: Show t => [t] -> IO ()
imprime l = do
    putStrLn (formataLista l)
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
                    ++ "(M)edicamentos\n"

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

dateTimeToString :: DateTime -> String
dateTimeToString dt =
    show (day dt) ++ "/" ++
    show (month dt) ++ "/" ++
    show (year dt) ++ " " ++
    show (hour dt) ++ ":" ++
    show (minute dt)

formataBool :: Bool -> String
formataBool True = "S"
formataBool False = "N"

lerReceita :: [(Int, String, Int)] -> IO [(Int, String, Int)]
lerReceita l = do
    putStr "Insira as informações dos medicamentos\n"

    idMedic <- prompt "ID do medicamento > "
    instru <- prompt "Instruções de uso > "
    qtd <- prompt "Quantidade de caixas > "

    op <- prompt "Deseja inserir outro? (S ou N) > "
    
    if toUpper (head op) == 'S' then do
        putStr "\n"
        lerReceita (l ++ [(read idMedic, instru, read qtd)])
    else do
        return (l ++ [(read idMedic, instru, read qtd)])

               