import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.Autenticacao as Autenticador
import Haskell.View.Utils

import Data.Char ( toUpper )

main :: IO()
main = do
    login (BD.BD [] [] [] [] [] [] [] [] [] [1..])

login :: BD.BD -> IO()
login dados  = do
    cadastra dados

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
        login dados {BD.pacientes = (BD.pacientes dados) ++ [PC.criaPaciente ([show (head (BD.idAtual dados))] ++ dadosP)], BD.logins = (BD.logins dados) ++ [(head (BD.idAtual dados), senha)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        login dados {BD.ubs = (BD.ubs dados) ++ [UBSC.criaUBS ([show (head (BD.idAtual dados))] ++ dadosU)], BD.logins = (BD.logins dados) ++ [(head (BD.idAtual dados), senha)], BD.idAtual = drop 1 (BD.idAtual dados)}

    else do
        clear
        cadastra dados