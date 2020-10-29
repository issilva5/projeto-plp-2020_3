import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.AutenticacaoController as Autenticador
import Haskell.View.Utils

import Data.Char ( toUpper )

main :: IO()
main = do
    login (BD.BD [] [] [] [] [] [] [] [] [] [1..])

login :: BD.BD -> IO()
login dados  = do
    cadastra dados

menuMedico :: Int -> BD.BD -> IO()
menuMedico idMed dados = do
    putStrLn medicoMenu
    op <- prompt "Opção > "

    putStrLn ""

    if toUpper (head op) == 'I' then do
        horario <- prompt "Horário > "
        putStrLn MC.informarHorario idMed horario BD.medicos
        menuMedico idMed dados
    
    else if toUpper (head op) == 'A' then do
        putStrLn medicoAcessarDados
        acessarOp <- prompt "Opção > "

        if toUpper (head acessarOp) == 'P' then do
            idPac <- prompt "ID do Paciente > "
            putStrLn MC.acessarDadosPaciente BD.pacientes idPac
            menuMedico idMed dados

        else if toUpper (head acessarOp) == 'E' then do
            idExame <- prompt "ID do Exame > "
            putStrLn MC.acessarExame idMed idExame BD.exames
            menuMedico idMed dados

        else if toUpper (head acessarOp) == 'A' then do
            date <- prompt "Data > "
            putStrLn MC.acessarConsultasData idMed date (BD.consultas dados)
            menuMedico idMed dados

        else do
            clear
    
    else if toUpper (head op) == 'E' then do
        putStrLn medicoEmitir
        emitirOp <- prompt "Opção > "

        if toUpper (head emitirOp) == 'R' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "
            menuMedico idMed dados {BD.receitas = (BD.receitas dados) ++ [(MC.emitirReceita idMed idPac (read informacoes))]}

        else if toUpper (head emitirOp) == 'S' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "
            menuMedico idMed dados {BD.exames = (BD.exames dados) ++ [(MC.emitirExame idMed idPac (read informacoes))]}

        else if toUpper (head emitirOp) == 'L' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "
            menuMedico idMed dados {BD.laudos = (BD.laudos dados) ++ [(MC.emitirLaudo idMed idPac (read informacoes))]}

        else do
            clear

    else if toUpper (head op) == 'T' then do
        idUBS <- prompt "ID da UBS > "
        putStrLn MC.solicitarTransferencia idMed idUBS
        menuMedico idMed dados

    else do
        clear


cadastra :: BD.BD -> IO()
cadastra dados = do
    putStrLn titleCadastro
    putStrLn "(P)aciente"
    putStrLn "(U)BS"
    putStrLn "(M)édico"
    op <- prompt "Opção > "
    
    putStrLn ""

    if toUpper (head op) == 'P' then do
        dadosP <- lePaciente
        senha <- prompt "Senha > "
        login dados {BD.pacientes = (BD.pacientes dados) ++ [PC.criaPaciente ([show (BD.nextID dados)] ++ dadosP)], BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 0)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        login dados {BD.ubs = (BD.ubs dados) ++ [UBSC.criaUBS ([show (BD.nextID dados)] ++ dadosU)], BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 1)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else do
        clear
        cadastra dados
