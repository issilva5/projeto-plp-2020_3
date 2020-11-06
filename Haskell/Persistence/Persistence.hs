module Haskell.Persistence.Persistence where

import qualified Haskell.Model.BD as BD
import Haskell.View.Utils ( split )

carregaPacientes :: BD.BD -> IO BD.BD
carregaPacientes dados = do
    pacientes <- leConteudo "pacientes.txt"
    carregaUBS dados {BD.pacientes =  BD.stringToPaciente $ split pacientes '\n' ""}

carregaUBS :: BD.BD -> IO BD.BD
carregaUBS dados = do
    ubs <- leConteudo "ubs.txt"
    carregaMedicos dados {BD.ubs =  BD.stringToUBS $ split ubs '\n' ""}

carregaMedicos :: BD.BD -> IO BD.BD
carregaMedicos dados = do
    medicos <- leConteudo "medicos.txt"
    carregaMedicamento dados {BD.medicos =  BD.stringToMedico $ split medicos '\n' ""}

carregaMedicamento :: BD.BD -> IO BD.BD
carregaMedicamento dados = do
    medicamentos <- leConteudo "medicamentos.txt"
    carregaLogins dados {BD.medicamentos =  BD.stringToMedicamento $ split medicamentos '\n' ""}

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
    putStrLn (show dados)

    let path = "Haskell/Persistence/"
    let listaPacientes = BD.pacientes dados
    let listaUBS =  BD.ubs dados
    let listaMedicos = BD.medicos dados
    let listaConsultas = BD.consultas dados
    let listaExames = BD.exames dados
    let listaLaudos = BD.laudos dados
    let listaMedicamentos = BD.medicamentos dados
    let listaReceitas = BD.receitas dados
    let listaLogins = BD.logins dados

    writeFile (path ++ "pacientes.txt") (BD.pacientesToString listaPacientes "")
    writeFile (path ++ "ubs.txt") (BD.ubsToString listaUBS "")
    writeFile (path ++ "medicos.txt") (BD.medicosToString listaMedicos "")
    writeFile (path ++ "medicamentos.txt") (BD.medicamentosToString listaMedicamentos "")
    writeFile (path ++ "receitas.txt") (BD.receitasToString listaReceitas "")
    writeFile (path ++ "consultas.txt") (BD.consultasToString listaConsultas "")
    writeFile (path ++ "exames.txt") (BD.examesToString listaExames "")
    writeFile (path ++ "laudos.txt") (BD.laudosToString listaLaudos "")
    writeFile (path ++ "logins.txt") (BD.loginsToString listaLogins "")
    writeFile (path ++ "idAtual.txt") (show $ BD.idAtual dados)

write :: String -> String -> IO()
write content fileName = do
    writeFile ("Haskell/Persistence/" ++ fileName) content


