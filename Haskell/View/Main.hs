import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.AutenticacaoController as Autenticador
import Haskell.View.Utils

import Data.Char ( toUpper )
import Data.Maybe (fromJust)
import Data.Dates

main :: IO()
main = do
    inicial (BD.BD [] [] [] [] [] [] [] [] [] [1..])

inicial :: BD.BD -> IO()
inicial dados  = do

    putStrLn (title "") 
    putStrLn (menuInicial "")

    op <- prompt ("Opção > ")

    if toUpper (head op) == 'L' then do
        login dados
    else if toUpper (head op) == 'C' then do
        cadastra dados
    else if toUpper (head op) == 'E' then do
        return ()
    else do
        inicial dados

login :: BD.BD -> IO()
login dados = do 
    
    putStrLn titleLogin
    id <- prompt ("Informe o id > ")
    senha <- prompt ("Informe a senha > ")
    putStrLn ""

    let aut = Autenticador.autentica (BD.logins dados) (read id) senha

    if aut == 0 then do
        clear
        menuPaciente (read id) dados 

    else if aut == 1 then do
        clear
        menuUBS (read id) dados 
    
    else if aut == 2 then do
        clear
        menuMedico (read id) dados
    
    else do
        clear
        inicial dados

cadastra :: BD.BD -> IO()
cadastra dados = do

    putStrLn titleCadastro
    putStrLn "(P)aciente"
    putStrLn "(U)BS"
    putStrLn "(V)oltar"
    op <- prompt "Opção > "
    
    putStrLn ""
    if toUpper (head op) == 'P' then do
        dadosP <- lePaciente
        senha <- prompt "Senha > "
        print ("Paciente cadastrado com id " ++ (show (BD.nextID dados)))
        inicial dados {BD.pacientes =
            (BD.pacientes dados) ++ [PC.criaPaciente (BD.nextID dados) dadosP], BD.logins =
                (BD.logins dados) ++ [(BD.nextID dados, senha, 0)], BD.idAtual =
                    drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        print ("UBS cadastrada com id " ++ (show (BD.nextID dados)))
        inicial dados {BD.ubs =
            (BD.ubs dados) ++ [UBSC.criaUBS (BD.nextID dados) dadosU], BD.logins =
                (BD.logins dados) ++ [(BD.nextID dados, senha, 1)], BD.idAtual =
                    drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'V' then do

        clear
        inicial dados

    else do
        clear
        inicial dados


menuPaciente :: Int -> BD.BD -> IO()
menuPaciente idPac dados = do

    putStrLn titlePaciente
    putStrLn "(B)uscar unidade por especialidade"
    putStrLn "(R)equisitar"
    putStrLn "(C)onsultar"
    putStrLn "(E)mergência"
    putStrLn "(S)air"

    op <- prompt "Opção > "

    if toUpper (head op) == 'B' then do

        leituraBuscaUnidades dados
        menuPaciente idPac dados

    else if toUpper (head op) == 'R' then do
        putStrLn "(C)onsulta"
        putStrLn "(E)xame"
        putStrLn "(M)edicamento"

        op <- prompt "Opção > "

        if toUpper (head op) == 'C' then do
            leituraRequisitaConsulta dados idPac
        else if toUpper (head op) == 'E' then do
            leituraRequisitaExame dados idPac
        else if toUpper (head op) == 'M' then do
            leituraRequisitaMedicamento dados idPac
        else do
            clear
            menuPaciente idPac dados

    else if toUpper (head op) == 'C' then do
        putStrLn "(L)audo"
        putStrLn "(R)eceita"

        op <- prompt "Opção > "

        if toUpper (head op) == 'L' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"

            if toUpper (head op) == 'T' then do
                leituraConsultaLaudo dados idPac
            else if toUpper (head op) == 'E' then do
                leituraConsultaLaudoId dados idPac
            else do
                clear
                menuPaciente idPac dados

        else if toUpper (head op) == 'R' then do

            putStrLn "(M)edicamento"
            putStrLn "(E)xame"

            if toUpper (head op) == 'M' then do

                putStrLn "(T)odos"
                putStrLn "(E)specífico"

                if toUpper (head op) == 'T' then do
                    leituraConsultaReceitaMedicamento dados idPac
                else if toUpper (head op) == 'E' then do
                    leituraConsultaReceitaMedicamentoId dados idPac
                else do
                    clear
                    menuPaciente idPac dados

            else if toUpper (head op) == 'E' then do

                putStrLn "(T)odos"
                putStrLn "(E)specífico"

                if toUpper (head op) == 'T' then do
                    leituraConsultaReceitaExame dados idPac
                else if toUpper (head op) == 'E' then do
                    leituraConsultaReceitaExameId dados idPac
                else do
                    clear
                    menuPaciente idPac dados
            
            else do
                clear
                menuPaciente idPac dados

        else do
            clear
            menuPaciente idPac dados

    else if toUpper (head op) == 'E' then do

        leituraEmergencia dados idPac
        menuPaciente idPac dados 

    else if toUpper (head op) == 'S' then do

        clear
        inicial dados

    else do
        clear
        menuPaciente idPac dados


menuUBS :: Int -> BD.BD -> IO()
menuUBS idUBS dados = do 
    putStrLn titleUBS
    op <- opcoesUBS

    if toUpper (head op) == 'C' then do
        
        dadosM <- leMedico
        senha <- prompt "Senha > "
        let med = UBSC.cadastraMedico idUBS (BD.nextID dados) dadosM
        print ("Médico cadastrado com id " ++ (show (BD.nextID dados)))
        menuUBS idUBS (dados {BD.medicos = [med] ++ (BD.medicos dados), BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 2)], BD.idAtual = drop 1 (BD.idAtual dados)})

    else if toUpper (head op) == 'V' then do
        op2 <- opcoesUBSVisualizar

        if toUpper (head op2) == 'A' then do
            hj <- getCurrentDateTime
            imprime (UBSC.visualizaAgendamentos idUBS (BD.consultas dados) hj)
            menuUBS idUBS dados
        
        else if toUpper (head op2) == 'P' then do
            hj <- getCurrentDateTime
            imprime (UBSC.visualizaPacientes idUBS (BD.pacientes dados) (UBSC.visualizaAgendamentos idUBS (BD.consultas dados) hj))
            menuUBS idUBS dados

        else if toUpper (head op2) == 'M' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do

                imprime (UBSC.visualizaMedicos idUBS (BD.medicos dados))
                menuUBS idUBS dados

            else if toUpper (head op3) == 'E' then do

                idMed <- prompt "ID do Médico > "

                if (MC.validaIDMedico (read idMed) (BD.medicos dados)) then do

                    print (fromJust (UBSC.visualizaMedico idUBS (read idMed) (BD.medicos dados)))
                    menuUBS idUBS dados
                
                else do

                    putStrLn "ID informado é inválido!"
                    menuUBS idUBS dados

            else do
                clear
                menuUBS idUBS dados

        else do menuUBS idUBS dados

    else if toUpper (head op) == 'F' then do
        op2 <- opcoesUBSFarmacia

        if toUpper (head op2) == 'C' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do

                imprime (UBSC.visualizaMedicamentos idUBS (BD.medicamentos dados))
                menuUBS idUBS dados

            else if toUpper (head op3) == 'E' then do

                idMedic <- prompt "ID do Medicamento > "

                if (UBSC.validaIDMedicamento (read idMedic) (BD.medicamentos dados)) then do

                    print (fromJust (UBSC.visualizaMedicamento idUBS (read idMedic) (UBSC.visualizaMedicamentos idUBS (BD.medicamentos dados))))
                    menuUBS idUBS dados
                
                else do

                    putStrLn "ID informado é inválido!"
                    menuUBS idUBS dados

            else do
                clear
                menuUBS idUBS dados

        else if toUpper (head op2) == 'N' then do
            
            dadosMedic <- leMedicamento
            let medic = UBSC.adicionaMedicamento idUBS ([show (BD.nextID dados)] ++ dadosMedic)
            print ("Medicamento criado com ID " ++ (show (BD.nextID dados)))
            menuUBS idUBS (dados {BD.medicamentos = [medic] ++ (BD.medicamentos dados), BD.idAtual = drop 1 (BD.idAtual dados)})

        else if toUpper (head op2) == 'A' then do

            idMedic <- prompt "ID do Medicamento > "
            quantToAdd <- prompt "Quantidade a adicionar > "

            if (UBSC.validaIDMedicamento (read idMedic) (BD.medicamentos dados)) then do

                let medic = UBSC.adicionaMedicamentoEstoque idUBS (read idMedic) (read quantToAdd) (BD.medicamentos dados)
                menuUBS idUBS dados {BD.medicamentos = medic}
            
            else do

                putStrLn "ID informado é inválido!"
                menuUBS idUBS dados

        else if toUpper (head op2) == 'R' then do
            
            idMedic <- prompt "ID do Medicamento > "
            quantToAdd <- prompt "Quantidade a remover > "

            if (UBSC.validaIDMedicamento (read idMedic) (BD.medicamentos dados)) then do

                let medic = UBSC.removerMedicamento idUBS (read idMedic) (read quantToAdd) (BD.medicamentos dados)
                menuUBS idUBS dados {BD.medicamentos = medic}
            
            else do

                putStrLn "ID informado é inválido!"
                menuUBS idUBS dados

        else do menuUBS idUBS dados

    else if toUpper (head op) == 'D' then do

        print "Dashboard"
        menuUBS idUBS dados

    else if toUpper (head op) == 'S' then do

        clear
        inicial dados

    else do
        clear
        menuUBS idUBS dados

menuMedico :: Int -> BD.BD -> IO()
menuMedico idMed dados = do

    let idUBS = (MC.getUBS idMed (BD.medicos dados))

    putStrLn medicoMenu
    op <- prompt "Opção > "

    putStrLn ""

    if toUpper (head op) == 'I' then do
        horario <- leHorariosMedico
        tempoConsulta <- prompt "Duração da consulta > "
        let med = MC.informarHorario idMed (fst horario) (snd horario) (read tempoConsulta) (BD.medicos dados)
        menuMedico idMed dados {BD.medicos = med}
    
    else if toUpper (head op) == 'A' then do
        putStrLn medicoAcessarDados
        acessarOp <- prompt "Opção > "

        if toUpper (head acessarOp) == 'P' then do

            idPac <- prompt "ID do Paciente > "

            if (PC.validaIDPaciente (read idPac) (BD.pacientes dados)) then do

                print (MC.acessarDadosPaciente (BD.pacientes dados) (read idPac))
                menuMedico idMed dados
            
            else do

                putStrLn "ID informado é inválido!"
                menuMedico idMed dados

        else if toUpper (head acessarOp) == 'E' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do

                imprime (MC.acessarExames idMed (BD.exames dados))
                menuMedico idMed dados

            else if toUpper (head op3) == 'E' then do

                putStrLn "ID do (E)xame"
                putStrLn "ID do (P)aciente"
                op4 <- prompt "Opção > "

                if toUpper (head op4) == 'E' then do

                    idExame <- prompt "ID do Exame > "

                    if (UBSC.validaIDExame (read idExame) (BD.exames dados)) then do

                        print (MC.acessarExame (read idExame) (BD.exames dados))
                        menuMedico idMed dados
                    
                    else do

                        putStrLn "ID informado é inválido!"
                        menuMedico idMed dados
                
                else if toUpper (head op4) == 'P' then do

                    idPaciente <- prompt "ID do Paciente > "

                    if (PC.validaIDPaciente (read idPaciente) (BD.pacientes dados)) then do

                        imprime (MC.acessarExamesPaciente (read idPaciente) (BD.exames dados))
                        menuMedico idMed dados
                    
                    else do

                        putStrLn "ID informado é inválido!"
                        menuMedico idMed dados
                
                else do
                    clear
                    menuMedico idMed dados
            
            else do
                clear
                menuMedico idMed dados

        else if toUpper (head acessarOp) == 'A' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do

                imprime (MC.acessarConsultas idMed (BD.consultas dados))
                menuMedico idMed dados
            
            else if toUpper (head op3) == 'E' then do

                date <- prompt "Data > "
                imprime (MC.acessarConsultasData idMed (read date) (BD.consultas dados))
                menuMedico idMed dados
            
            else do
                clear
                menuMedico idMed dados

        else do
            clear
            menuMedico idMed dados
    
    else if toUpper (head op) == 'E' then do
        putStrLn medicoEmitir
        emitirOp <- prompt "Opção > "

        if toUpper (head emitirOp) == 'R' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "

            if (PC.validaIDPaciente (read idPac) (BD.pacientes dados)) then do

                menuMedico idMed dados {BD.receitas = (BD.receitas dados) ++ [(MC.emitirReceita (BD.nextID dados) idMed (read idPac) idUBS (read informacoes))], BD.idAtual = drop 1 (BD.idAtual dados)}

            else do

                putStrLn "ID informado é inválido!"
                menuMedico idMed dados

        else if toUpper (head emitirOp) == 'E' then do

            idExame <- prompt "ID do Exame > "
            resultado <- prompt "Resultado > "

            if (UBSC.validaIDExame (read idExame) (BD.exames dados)) then do

                menuMedico idMed dados {BD.exames = MC.emitirResultadoExame (BD.exames dados) (read idExame) resultado}

            else do

                    putStrLn "ID informado é inválido!"
                    menuMedico idMed dados

        else if toUpper (head emitirOp) == 'L' then do
            idExame <- prompt "ID do Exame > "
            informacoes <- prompt "Informações > "

            if (UBSC.validaIDExame (read idExame) (BD.exames dados)) then do

                menuMedico idMed dados {BD.laudos = (BD.laudos dados) ++ [(MC.emitirLaudo (BD.nextID dados) idMed (read idExame) (read informacoes))], BD.idAtual = drop 1 (BD.idAtual dados)}

            else do

                putStrLn "ID informado é inválido!"
                menuMedico idMed dados

        else do
            clear
            menuMedico idMed dados

    else if toUpper (head op) == 'T' then do
        input <- prompt "ID da UBS > "

        if (UBSC.validaIDUBS (read input) (BD.ubs dados)) then do

            let idUBS = read input :: Int
            let med = MC.solicitarTransferencia idMed idUBS (BD.medicos dados)
            menuMedico idMed dados {BD.medicos = med}
        
        else do

            putStrLn "ID informado é inválido!"
            menuMedico idMed dados

    else if toUpper (head op) == 'S' then do

        clear
        inicial dados

    else do
        clear
        menuMedico idMed dados

leituraBuscaUnidades :: BD.BD -> IO ()
leituraBuscaUnidades dados = do
    especialidade <- prompt "Especialidade > "
    imprime (PC.buscarUnidades especialidade (BD.medicos dados) (BD.ubs dados))

leituraRequisitaConsulta :: BD.BD -> Int -> IO()
leituraRequisitaConsulta dados idPaciente = do
    aux <- sequence [prompt "ID do Médico > ", prompt "ID da UBS > "]
    let informs = [(show (BD.nextID dados)), (show idPaciente)] ++ aux

    if (UBSC.validaIDUBS (read (aux !! 1)) (BD.ubs dados)) && (MC.validaIDMedico (read (aux !! 0)) (BD.medicos dados)) then do

        let medicoHorario = MC.proximoHorarioLivre (read (head informs) :: Int) (BD.medicos dados)
        print ("Consulta marcada com id " ++ (show (BD.nextID dados)))
        menuPaciente idPaciente (dados {BD.consultas = (BD.consultas dados) ++ [PC.requisitarConsulta informs (fst medicoHorario)], BD.idAtual = drop 1 (BD.idAtual dados), BD.medicos = (snd medicoHorario)})

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraRequisitaExame :: BD.BD -> Int -> IO()
leituraRequisitaExame dados idPaciente = do  
    aux <- sequence [prompt "ID do Médico > ", prompt "ID da UBS > ", prompt "Tipo de Exame > "]
    let informs = [(show (BD.nextID dados)), (show idPaciente)] ++ aux

    if (UBSC.validaIDUBS (read (aux !! 1)) (BD.ubs dados)) && (MC.validaIDMedico (read (aux !! 0)) (BD.medicos dados)) then do

        let medicoHorario = MC.proximoHorarioLivre (read (head informs) :: Int) (BD.medicos dados)
        print ("Exame marcado com id " ++ (show (BD.nextID dados)))
        menuPaciente idPaciente (dados {BD.exames = (BD.exames dados) ++ [PC.requisitarExame informs (fst medicoHorario)], BD.idAtual = drop 1 (BD.idAtual dados), BD.medicos = (snd medicoHorario)})

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraRequisitaMedicamento :: BD.BD -> Int -> IO()
leituraRequisitaMedicamento dados idPaciente = do  
    idReceita <- prompt "ID da Receita > "

    if (UBSC.validaIDReceita (read idReceita) (BD.receitas dados)) then do

        menuPaciente idPaciente (dados {BD.medicamentos = PC.requisitarMedicamento (read idReceita) (BD.receitas dados) (BD.medicamentos dados)})

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraConsultaLaudo :: BD.BD -> Int -> IO()
leituraConsultaLaudo dados idPaciente = do
    imprime (PC.consultarLaudos idPaciente (BD.laudos dados))
    menuPaciente idPaciente dados

leituraConsultaLaudoId :: BD.BD -> Int -> IO()
leituraConsultaLaudoId dados idPaciente = do
    idLaudo <- prompt "ID do Laudo > "

    if (UBSC.validaIDLaudo (read idLaudo) (BD.laudos dados)) then do

        print (PC.consultarLaudo idPaciente (read idLaudo) (BD.laudos dados))
        menuPaciente idPaciente dados

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraConsultaReceitaMedicamento :: BD.BD -> Int -> IO()
leituraConsultaReceitaMedicamento dados idPaciente = do
    imprime (PC.consultarReceitasMed idPaciente (BD.receitas dados))
    menuPaciente idPaciente dados

leituraConsultaReceitaMedicamentoId :: BD.BD -> Int -> IO()
leituraConsultaReceitaMedicamentoId dados idPaciente = do
    idReceita <- prompt "ID da Receita > "

    if (UBSC.validaIDReceita (read idReceita) (BD.receitas dados)) then do

        print (PC.consultarReceitaMed idPaciente (read idReceita) (BD.receitas dados))
        menuPaciente idPaciente dados

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraConsultaReceitaExame :: BD.BD -> Int -> IO()
leituraConsultaReceitaExame dados idPaciente = do
    imprime (PC.consultarReceitasEx idPaciente (BD.exames dados))
    menuPaciente idPaciente dados

leituraConsultaReceitaExameId :: BD.BD -> Int -> IO()
leituraConsultaReceitaExameId dados idPaciente = do
    exame <- prompt "ID do Exame > "

    if (UBSC.validaIDExame (read exame) (BD.exames dados)) then do

        print (PC.consultarReceitaEx idPaciente (read exame) (BD.exames dados))
        menuPaciente idPaciente dados

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraEmergencia :: BD.BD -> Int -> IO ()
leituraEmergencia dados idPac = do
    endereco <- prompt "Endereço > "
    print (PC.emergencia idPac endereco)