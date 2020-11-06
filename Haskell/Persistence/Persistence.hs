module Haskell.Persistence.Persistence where

import qualified Haskell.Model.BD as BD
import Haskell.View.Utils ( split )
import System.IO

carregaPacientes :: BD.BD -> IO BD.BD
carregaPacientes dados = do
    pacientes <- leConteudo "pacientes.txt"
    carregaUBS dados {BD.pacientes =  BD.stringToPaciente $ split pacientes '\n' ""}

carregaUBS :: BD.BD -> IO BD.BD
carregaUBS dados = do
    ubs <- leConteudo "ubs.txt"
    carregaLogins dados {BD.ubs =  BD.stringToUBS $ split ubs '\n' ""}

carregaMedicos :: BD.BD -> IO BD.BD
carregaMedicos dados = do return dados
    --medicos <- leConteudo "medicos.txt"
    --carregaConsultas dados {BD.medicos =  BD.stringToMedico $ split medicos '\n' ""}

carregaLogins :: BD.BD -> IO BD.BD
carregaLogins dados = do
    logins <- leConteudo "logins.txt"
    carregaIdAtual dados {BD.logins = BD.stringToLogin $ split logins '\n' ""}

carregaIdAtual :: BD.BD -> IO BD.BD
carregaIdAtual dados = do
    id <- leConteudo "idAtual.txt"
    return (dados {BD.idAtual = read id})

leConteudo :: String -> IO String
leConteudo fileName = readFile ("Haskell/Persistence/" ++ fileName)

encerrar :: BD.BD -> IO()
encerrar dados = do
    print dados
    let medicos = BD.medicamentos dados
    let consultas = BD.consultas dados
    let exames = BD.exames dados
    let laudos = BD.laudos dados
    let medicamentos = BD.medicamentos dados
    let receitas = BD.receitas dados
    let loins = BD.logins dados

    write (BD.pacientesToString (BD.pacientes dados) "") "pacientes.txt"
    write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    -- write (BD.ubsToString (BD.ubs dados) "") "ubs.txt"
    write (BD.loginsToString (BD.logins dados) "") "logins.txt"
    write (show (BD.idAtual dados)) "idAtual.txt"


write :: String -> String -> IO()
write content fileName = do
    arq <- openFile ("Haskell/Persistence/" ++ fileName) WriteMode
    hPutStrLn arq content
    hClose arq

