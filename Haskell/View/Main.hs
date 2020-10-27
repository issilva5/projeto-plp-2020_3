import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.Autenticacao as Autenticador
import Haskell.View.Utils

import Data.Char ( toUpper )

main :: IO()
main = do
    login (BD.BD [] [] [] [] [] [] [] [] [])

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
        clear
        login dados {BD.pacientes = (BD.pacientes dados) ++ [PC.criaPaciente dadosP]}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        clear
        login dados {BD.ubs = (BD.ubs dados) ++ [UBSC.criaUBS dadosU]}

    else do
        clear
        cadastra dados