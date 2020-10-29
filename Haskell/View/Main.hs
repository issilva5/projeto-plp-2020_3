import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.AutenticacaoController as Autenticador
import Haskell.View.Utils

import Data.Char ( toUpper )

main :: IO()
main = do
    inicial (BD.BD [] [] [] [] [] [] [] [] [] [1..])

inicial :: BD.BD -> IO()
inicial dados  = do

    putStrLn (title "") 
    putStrLn (menuInicial "")

    op <- prompt ("Opção > ")

    let acao | toUpper (head op) == 'L' = login
             | toUpper (head op) == 'C' = cadastra
             | otherwise = inicial

    acao dados

login :: BD.BD -> IO()
login dados = do 
    
    id <- prompt ("Informe o id > ")
    senha <- prompt ("Informe a senha > ")
    putStrLn "---------"

    let aut = Autenticador.autentica (BD.logins dados) id senha

    let proxMenu | aut == 0 = menuPaciente
                 | aut == 1 = menuUBS
                 | aut == 2 = menuMedico
                 | otherwise = login 

    proxMenu dados

cadastra :: BD.BD -> IO()
cadastra dados = do
    putStrLn titleCadastro
    putStrLn "(P)aciente"
    putStrLn "(U)BS"
    op <- prompt "Opção > "
    
    putStrLn ""

    if toUpper (head op) == 'P' then do
        dadosP <- lePaciente
        senha <- prompt "Senha > "
        inicial dados {BD.pacientes = (BD.pacientes dados) ++ [PC.criaPaciente (BD.nextID dados) dadosP], BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 0)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        inicial dados {BD.ubs = (BD.ubs dados) ++ [UBSC.criaUBS (BD.nextID dados) dadosU], BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 1)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else do
        clear
        inicial dados

menuPaciente :: BD.BD -> IO()
menuPaciente dados = do 
    putStrLn "Paciente"

menuUBS :: BD.BD -> IO()
menuUBS dados = do 
    putStrLn "UBS"

menuMedico :: BD.BD -> IO()
menuMedico dados = do 
    putStrLn "Medico"