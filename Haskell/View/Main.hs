import qualified Haskell.Model.BD as BD
import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Controller.UBSController as UBSC
import qualified Haskell.Controller.PacienteController as PC
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

    if aut == 0 then
        menuPaciente (read id) dados

    else if aut == 1 then
        menuUBS (read id) dados
    
    else if aut == 2 then
        menuMedico (read id) dados
    
    else do
        inicial dados

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
        inicial dados {BD.pacientes =
            (BD.pacientes dados) ++ [PC.criaPaciente (BD.nextID dados) dadosP], BD.logins =
                (BD.logins dados) ++ [(BD.nextID dados, senha, 0)], BD.idAtual =
                    drop 1 (BD.idAtual dados)}

    else if toUpper (head op) == 'U' then do
        dadosU <- leUBS
        senha <- prompt "Senha > "
        inicial dados {BD.ubs =
            (BD.ubs dados) ++ [UBSC.criaUBS (BD.nextID dados) dadosU], BD.logins =
                (BD.logins dados) ++ [(BD.nextID dados, senha, 1)], BD.idAtual =
                    drop 1 (BD.idAtual dados)}

    else do
        clear
        inicial dados

{-

Interface do paciente, deve realizar as operações descritas na API

-}

leituraBuscaUnidades :: BD.BD -> IO [String]
leituraBuscaUnidades dados = do
    especialidade <- prompt "Especialidade > "
    print PC.buscarUnidades especialidade (BD.ubs dados)

leituraRequisitaConsulta :: BD.BD -> Int -> IO()
leituraRequisitaConsulta dados idPaciente = do
    let informs = sequence [prompt "ID do Médico > ", prompt "ID da UBS > "]

    -- Validar IDS

    let medicoHorario = MC.proximoHorarioLivre (read (head informs) :: Int) (BD.medicos dados)

    menuPaciente dados {BD.consultas = (BD.consultas dados) ++ [PC.requisitarConsulta (BD.nextId BD) idPaciente informs (fst medicoHorario)], BD.idAtual = drop 1 (BD.idAtual dados), BD.medicos = (snd medicoHorario)} 

leituraRequisitaExame :: BD.BD -> Int -> IO()
leituraRequisitaExame dados idPaciente = do  
    let informs = sequence [prompt "ID do Médico > ", prompt "ID da UBS > ", prompt "Tipo de Exame > "]

    -- Validar IDS

    let medicoHorario = MC.proximoHorarioLivre (read (head informs) :: Int) (BD.medicos dados)

    menuPaciente dados {BD.exames = (BD.consultas dados) ++ [PC.requisitarExame (BD.nextId BD) idPaciente informs (fst medicoHorario)], BD.idAtual = drop 1 (BD.idAtual dados), BD.medicos = (snd medicoHorario)} 

leituraRequisitaMedicamento :: BD.BD -> Int -> IO()
leituraRequisitaExame dados isPaciente = do  
    idReceita <- prompt "ID da Receita > "

    -- Get receita pelo ID

    menuPaciente dados {dados.medicamentos = (BD.consultas dados) ++ [PC.requisitarMedicamento receita (BD.medicamentos dados)]} 

leituraConsultaLaudo :: BD.BD -> Int -> IO()
leituraConsultaLaudo dados idPaciente = do
    print PC.consultarLaudos idPac (BD.laudos dados)

leituraConsultaLaudoId :: BD.BD -> Int -> IO()
leituraConsultaLaudoId dados idPaciente = do
    idLaudo <- prompt "ID do Laudo > "

    -- Validar ID

    print PC.consultarLaudo idPac idLaudo (BD.laudos dados)

leituraConsultaReceitaMedicamento :: BD.BD -> Int -> IO()
leituraConsultaReceitaMedicamento dados idPaciente = do
    print PC.consultarReceitasMed idPac (BD.receitas dados)

leituraConsultaReceitaMedicamentoNome :: BD.BD -> Int -> IO()
leituraConsultaReceitaMedicamentoNome dados idPaciente = do
    medicamento <- prompt "Nome do Medicamento > "

    -- Validar Medicamento

    print PC.consultarReceitaMed idPac medicamento (BD.receitas dados)

leituraConsultaReceitaExame :: BD.BD -> Int -> IO()
leituraConsultaReceitaExame dados idPaciente = do
    print PC.consultarReceitasEx idPac (BD.exames dados)

leituraConsultaReceitaExameNome :: BD.BD -> Int -> IO()
leituraConsultaReceitaExameId dados idPaciente = do
    exame <- prompt "Nome do Exame > "

    -- Validar Exame

    print PC.consultarReceitaEx idPac exame (BD.exames dados)

leituraEmergencia :: BD.BD -> Int -> IO [String]
leituraBuscaUnidades dados idPac = do
    endereco <- prompt "Endereço > "
    print PC.emergencia idPac endereco


menuPaciente :: BD.BD -> Int -> IO()
menuPaciente dados idPac = do
    putStrLn title
    putStrLn "(B)uscar unidade por especialidade"
    putStrLn "(R)equisitar"
    putStrLn "(C)onsultar"
    putStrLn "(E)mergência"

    op <- prompt "Opção > "

    if toUpper (head op) == 'B' then do
        leituraBuscaUnidades dados
        menuPaciente dados idPac
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
        else menuPaciente dados idPac

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
            else menuPaciente dados idPac
        else if toUpper (head op) == 'R' then do
            putStrLn "(M)edicamento"
            putStrLn "(E)xame"
            if toUpper (head op) == 'T' then do
                putStrLn "(T)odos"
                putStrLn "(E)specífico"
                if toUpper (head op) == 'T' then do
                    leituraConsultaReceitaMedicamento dados idPac
                else if toUpper (head op) == 'E' then do
                    leituraConsultaReceitaMedicamentoNome dados idPac
                else menuPaciente dados idPac
            else if toUpper (head op) == 'E' then do
                putStrLn "(T)odos"
                putStrLn "(E)specífico"
                if toUpper (head op) == 'T' then do
                    leituraConsultaReceitaExame dados idPac
                else if toUpper (head op) == 'E' then do
                    leituraConsultaReceitaExameNome dados idPac
                else menuPaciente dados idPac
            else menuPaciente dados idPac
        else menuPaciente dados idPac
    else if toUpper (head op) == 'E' then do
        leituraEmergencia dados idPac
        menuPaciente dados idPac
    else menuPaciente dados idPac

menuUBS :: Int -> BD.BD -> IO()
menuUBS idUBS dados = do 
    putStrLn titleUBS
    op <- opcoesUBS

    if toUpper (head op) == 'C' then do
        
        dadosM <- leMedico
        senha <- prompt "Senha > "
        let med = UBSC.cadastraMedico idUBS (BD.nextID dados) dadosM
        print ("Médico cadastrado com id " ++ (show (BD.nextID dados)))
        menuUBS idUBS (dados {BD.medicos = [med] ++ (BD.medicos dados), BD.logins = (BD.logins dados) ++ [(BD.nextID dados, senha, 1)], BD.idAtual = drop 1 (BD.idAtual dados)})

    else if toUpper (head op) == 'V' then do
        op2 <- opcoesUBSVisualizar

        if toUpper (head op2) == 'A' then do
            
            print (UBSC.visualizaAgendamentos idUBS (BD.consultas dados))
        
        else if toUpper (head op2) == 'P' then do
            
            print (UBSC.visualizaPacientes (BD.pacientes dados) (UBSC.visualizaAgendamentos idUBS (BD.consultas dados)))
        
        else if toUpper (head op2) == 'M' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do
                print (UBSC.visualizaMedicos idUBS (BD.medicos dados))
            else if toUpper (head op3) == 'E' then do
                idMed <- prompt "ID do Médico > "
                print (UBSC.visualizaMedico idUBS (read idMed) (BD.medicos dados))
            else do
                menuUBS idUBS dados

        else do menuUBS idUBS dados

    else if toUpper (head op) == 'F' then do
        op2 <- opcoesUBSFarmacia

        if toUpper (head op2) == 'C' then do

            putStrLn "(T)odos"
            putStrLn "(E)specífico"
            op3 <- prompt "Opção > "

            if toUpper (head op3) == 'T' then do

                print (UBSC.visualizaMedicamentos idUBS (BD.medicamentos dados))

            else if toUpper (head op3) == 'E' then do

                idMedic <- prompt "ID do Medicamento > "
                print (UBSC.visualizaMedicamento (read idMedic) (UBSC.visualizaMedicamentos idUBS (BD.medicamentos dados)))
            
            else do
                menuUBS idUBS dados

        else if toUpper (head op2) == 'N' then do
            
            dadosMedic <- leMedicamento
            let medic = UBSC.adicionaMedicamento idUBS (BD.nextID dados) dadosMedic
            menuUBS idUBS (dados {BD.medicamentos = [medic] ++ (BD.medicamentos dados), BD.idAtual = drop 1 (BD.idAtual dados)})

        else if toUpper (head op2) == 'A' then do

            idMedic <- prompt "ID do Medicamento > "
            quantToAdd <- prompt "Quantidade a adicionar > "
            let medic = UBSC.adicionaMedicamentoEstoque idUBS (read idMedic) (read quantToAdd) (BD.medicamentos dados)
            menuUBS idUBS dados {BD.medicamentos = medic}

        else if toUpper (head op2) == 'R' then do
            
            idMedic <- prompt "ID do Medicamento > "
            quantToAdd <- prompt "Quantidade a remover > "
            let medic = UBSC.removerMedicamento idUBS (read idMedic) (read quantToAdd) (BD.medicamentos dados)
            menuUBS idUBS dados {BD.medicamentos = medic}

        else do menuUBS idUBS dados

    else if toUpper (head op) == 'D' then do

        print "Dashboard"

    else if toUpper (head op) == 'S' then do

        inicial dados

    else do
        clear
        menuUBS idUBS dados

menuMedico :: Int -> BD.BD -> IO()
menuMedico idMed dados = do
    putStrLn medicoMenu
    op <- prompt "Opção > "

    putStrLn ""

    if toUpper (head op) == 'I' then do
        horario <- prompt "Horário > "
        putStrLn (show (MC.informarHorario idMed horario (BD.medicos dados)))
        menuMedico idMed dados
    
    else if toUpper (head op) == 'A' then do
        putStrLn medicoAcessarDados
        acessarOp <- prompt "Opção > "

        if toUpper (head acessarOp) == 'P' then do
            idPac <- prompt "ID do Paciente > "
            putStrLn (show (MC.acessarDadosPaciente (BD.pacientes dados) (read idPac)))
            menuMedico idMed dados

        else if toUpper (head acessarOp) == 'E' then do
            idExame <- prompt "ID do Exame > " 
            putStrLn (show (MC.acessarExame idMed (read idExame) (BD.exames dados)))
            menuMedico idMed dados

        else if toUpper (head acessarOp) == 'A' then do
            date <- prompt "Data > "
            putStrLn (show (MC.acessarConsultasData idMed date (BD.consultas dados)))
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
            menuMedico idMed dados {BD.receitas = (BD.receitas dados) ++ [(MC.emitirReceita idMed (read idPac) (read informacoes))]}

        else if toUpper (head emitirOp) == 'S' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "
            menuMedico idMed dados {BD.exames = (BD.exames dados) ++ [(MC.emitirExame idMed (read idPac) (read informacoes))]}

        else if toUpper (head emitirOp) == 'L' then do
            idPac <- prompt "ID do Paciente > "
            informacoes <- prompt "Informações > "
            menuMedico idMed dados {BD.laudos = (BD.laudos dados) ++ [(MC.emitirLaudo idMed (read idPac) (read informacoes))]}

        else do
            clear
            menuMedico idMed dados

    else if toUpper (head op) == 'T' then do
        input <- prompt "ID da UBS > "
        let idUBS = read input ::Int 
        putStrLn (show (MC.solicitarTransferencia idMed idUBS))
        menuMedico idMed dados

    else do
        clear
        menuMedico idMed dados
