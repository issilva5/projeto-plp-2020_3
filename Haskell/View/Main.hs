import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.PacienteController as PC
import qualified Haskell.Controller.AutenticacaoController as Autenticador
import Haskell.View.Utils

import Data.Dates
import Data.Char ( toUpper )
import Data.Maybe (fromJust, isNothing)
import qualified Haskell.Persistence.Persistence as Persistence
import System.IO (utf8, hSetEncoding, stdout)
import Data.List (sort)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    dados <- Persistence.carregaPacientes $ BD.BD [] [] [] [] [] [] [] [] [] 1
    writeFile "/tmp/foo" (show dados)
    inicial dados

inicial :: BD.BD -> IO()
inicial dados  = do

    putStrLn (title "")
    putStrLn (menuInicial "")

    op <- prompt ("Opção > ")

    if toUpper (head op) == 'L' then do
        clear
        login dados
    else if toUpper (head op) == 'C' then do
        clear
        cadastra dados
    else if toUpper (head op) == 'E' then do
        Persistence.encerrar dados
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
        print ("Paciente cadastrado com id " ++ (show (BD.idAtual dados)))
        inicial dados {BD.pacientes =
            (BD.pacientes dados) ++ [PC.criaPaciente (BD.idAtual dados) dadosP], BD.logins =
                (BD.logins dados) ++ [(BD.idAtual dados, senha, 0)], BD.idAtual =
                    1 + (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        print ("UBS cadastrada com id " ++ (show (BD.idAtual dados)))
        inicial dados {BD.ubs =
            (BD.ubs dados) ++ [UBSC.criaUBS (BD.idAtual dados) dadosU], BD.logins =
                (BD.logins dados) ++ [(BD.idAtual dados, senha, 1)], BD.idAtual =
                    1 + (BD.idAtual dados)}

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

        op2 <- prompt "Opção > "

        if toUpper (head op2) == 'C' then do
            leituraRequisitaConsulta dados idPac
        else if toUpper (head op2) == 'E' then do
            leituraRequisitaExame dados idPac
        else if toUpper (head op2) == 'M' then do
            leituraRequisitaMedicamento dados idPac
        else do
            clear
            menuPaciente idPac dados

    else if toUpper (head op) == 'C' then do
        putStrLn "(L)audo"
        putStrLn "(R)eceita"
        putStrLn "(C)onsultas"

        op2 <- prompt "Opção > "
        
        if toUpper (head op2) == 'L' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"

            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do
                leituraConsultaLaudo dados idPac
            else if toUpper (head op3) == 'E' then do
                leituraConsultaLaudoId dados idPac
            else do
                clear
                menuPaciente idPac dados

        else if toUpper (head op2) == 'R' then do

            putStrLn "(M)edicamento"
            putStrLn "(E)xame"

            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'M' then do

                putStrLn "(T)odos"
                putStrLn "(E)specífico"

                op3 <- prompt "Opção > "

                if toUpper (head op3) == 'T' then do
                    leituraConsultaReceitaMedicamento dados idPac
                else if toUpper (head op3) == 'E' then do
                    leituraConsultaReceitaMedicamentoId dados idPac
                else do
                    clear
                    menuPaciente idPac dados

            else if toUpper (head op3) == 'E' then do

                putStrLn "(T)odos"
                putStrLn "(E)specífico"

                op4 <- prompt "Opção > "

                if toUpper (head op4) == 'T' then do
                    leituraConsultaReceitaExame dados idPac
                else if toUpper (head op4) == 'E' then do
                    leituraConsultaReceitaExameId dados idPac
                else do
                    clear
                    menuPaciente idPac dados

            else do
                clear
                menuPaciente idPac dados
        else if toUpper (head op2) == 'C' then do

            imprime (PC.consultarConsultas idPac (BD.consultas dados))
            menuPaciente idPac dados

        else do
            clear
            menuPaciente idPac dados

    else if toUpper (head op) == 'E' then do

        leituraEmergencia
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
        let med = UBSC.cadastraMedico idUBS (BD.idAtual dados) dadosM
        putStrLn ("Médico cadastrado com id " ++ (show (BD.idAtual dados)))
        menuUBS idUBS (dados {BD.medicos = [med] ++ (BD.medicos dados), BD.logins = (BD.logins dados) ++ [(BD.idAtual dados, senha, 2)], BD.idAtual = 1 + (BD.idAtual dados)})

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
            let medic = UBSC.adicionaMedicamento idUBS ([show (BD.idAtual dados)] ++ dadosMedic)
            print ("Medicamento criado com ID " ++ (show (BD.idAtual dados)))
            menuUBS idUBS (dados {BD.medicamentos = [medic] ++ (BD.medicamentos dados), BD.idAtual = 1 + (BD.idAtual dados)})

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

        putStrLn "Dashboard[UBS]: Consultas"
        putStrLn "                Consultas do Dia: \n"
        hoje <- getCurrentDateTime
        imprime (UBSC.getConsultasDoDia hoje (UBSC.visualizaAgendamentosTodos idUBS (BD.consultas dados)))

        putStrLn "Dashboard[UBS]: Medicamentos"
        putStrLn "                Medicamentos com pouco estoque: \n"
        putStrLn (UBSC.formataMedicamentosDashboard (take 5 (sort $ UBSC.visualizaMedicamentos idUBS (BD.medicamentos dados))))

        putStrLn "Dashboard[UBS]: Médicos"
        putStrLn "                Status dos médicos: \n"
        hj <- getCurrentDateTime
        putStrLn (UBSC.formataMedicosDashboard (UBSC.getStatusMedicos hj (BD.consultas dados) (UBSC.visualizaMedicos idUBS (BD.medicos dados))))

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
        hj <- getCurrentDateTime
        let med = MC.informarHorario idMed hj (fst horario) (snd horario) (read tempoConsulta) (BD.medicos dados)
        menuMedico idMed dados {BD.medicos = med}

    else if toUpper (head op) == 'A' then do
        putStrLn medicoAcessarDados
        acessarOp <- prompt "Opção > "

        if toUpper (head acessarOp) == 'P' then do

            idPac <- prompt "ID do Paciente > "

            if (PC.validaIDPaciente (read idPac) (BD.pacientes dados)) then do

                print (fromJust (MC.acessarDadosPaciente (BD.pacientes dados) (read idPac)))
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

                        print (fromJust (MC.acessarExame (read idExame) (BD.exames dados)))
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
        
        else if toUpper (head acessarOp) == 'M' then do
            
            imprime (BD.medicamentos dados)
            menuMedico idMed dados
        
        else do
            clear
            menuMedico idMed dados

    else if toUpper (head op) == 'E' then do
        putStrLn medicoEmitir
        emitirOp <- prompt "Opção > "

        if toUpper (head emitirOp) == 'R' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- lerReceita []

            if (PC.validaIDPaciente (read idPac) (BD.pacientes dados)) && (UBSC.validaReceita informacoes (BD.medicamentos dados)) then do

                menuMedico idMed dados {BD.receitas = (BD.receitas dados) ++ [(MC.emitirReceita (BD.idAtual dados) idMed (read idPac) idUBS informacoes)], BD.idAtual = 1 + (BD.idAtual dados)}

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

                menuMedico idMed dados {BD.laudos = (BD.laudos dados) ++ [(MC.emitirLaudo (BD.idAtual dados) idMed (read idExame) [informacoes])], BD.idAtual = 1 + (BD.idAtual dados)}

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
    imprime (PC.buscarUnidades especialidade (BD.medicos dados))

leituraRequisitaConsulta :: BD.BD -> Int -> IO()
leituraRequisitaConsulta dados idPaciente = do
    aux <- sequence [prompt "ID do Médico > ", prompt "ID da UBS > "]
    let informs = [(show (BD.idAtual dados)), (show idPaciente)] ++ aux

    if (UBSC.validaIDUBS (read (aux !! 1)) (BD.ubs dados)) && (MC.validaIDMedico (read (aux !! 0)) (BD.medicos dados)) then do

        hj <- getCurrentDateTime
        let medicoHorario = MC.proximoHorarioLivre (read (aux !! 0)) hj (BD.medicos dados)
        
        if isNothing (fst medicoHorario) then do
            putStrLn "Consulta não pôde ser marcada, médico indisponível!"
            menuPaciente idPaciente dados
        else do
            print ("Consulta marcada com id " ++ (show (BD.idAtual dados)))
            menuPaciente idPaciente (dados {BD.consultas = (BD.consultas dados) ++ [PC.requisitarConsulta informs (fromJust (fst medicoHorario))], BD.idAtual = 1 + (BD.idAtual dados), BD.medicos = (snd medicoHorario)})

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraRequisitaExame :: BD.BD -> Int -> IO()
leituraRequisitaExame dados idPaciente = do
    aux <- sequence [prompt "ID do Médico > ", prompt "ID da UBS > ", prompt "Tipo de Exame > "]
    let informs = [(show (BD.idAtual dados)), (show idPaciente)] ++ aux

    if (UBSC.validaIDUBS (read (aux !! 1)) (BD.ubs dados)) && (MC.validaIDMedico (read (aux !! 0)) (BD.medicos dados)) then do

        hj <- getCurrentDateTime
        let medicoHorario = MC.proximoHorarioLivre (read (aux !! 0)) hj (BD.medicos dados)
        
        if isNothing (fst medicoHorario) then do
            putStrLn "Exame não pôde ser marcado, médico indisponível!"
            menuPaciente idPaciente dados
        else do
            print ("Exame marcado com id " ++ (show (BD.idAtual dados)))
            menuPaciente idPaciente (dados {BD.exames = (BD.exames dados) ++ [PC.requisitarExame informs (fromJust (fst medicoHorario))], BD.idAtual = 1 + (BD.idAtual dados), BD.medicos = (snd medicoHorario)})

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
    imprime (PC.consultarLaudos idPaciente (BD.laudos dados) (BD.exames dados))
    menuPaciente idPaciente dados

leituraConsultaLaudoId :: BD.BD -> Int -> IO()
leituraConsultaLaudoId dados idPaciente = do
    idLaudo <- prompt "ID do Laudo > "

    if (UBSC.validaIDLaudo (read idLaudo) (BD.laudos dados)) then do

        print (fromJust (PC.consultarLaudo (read idLaudo) (BD.laudos dados)))
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

        print (fromJust (PC.consultarReceitaMed (read idReceita) (BD.receitas dados)))
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

        print (fromJust (PC.consultarReceitaEx (read exame) (BD.exames dados)))
        menuPaciente idPaciente dados

    else do

        putStrLn "ID informado é inválido!"
        menuPaciente idPaciente dados

leituraEmergencia :: IO ()
leituraEmergencia = do
    endereco <- prompt "Endereço > "
    putStrLn (PC.emergencia endereco)