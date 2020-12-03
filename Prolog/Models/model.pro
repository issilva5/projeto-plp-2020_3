:- module(model, [iniciaPaciente/0, iniciaUBS/0, iniciaMedico/0,
                  iniciaReceita/0, iniciaMedicamento/0, iniciaLaudo/0,
                  iniciaExame/0, iniciaId/0, iniciaLogins/0, iniciaConsulta/0,
                  iniciaSistema/0, nextId/1]).

:- use_module('../Persistence/persistence.pro').

/*

Inicializa a tabela de pacientes.
Os campos são: id :: Int, nome :: String, cpf :: String,
dataNascimento :: String, peso :: Double, altura :: Double,
tipoSanguineo :: String, endereco :: String, cardiopata :: Bool,
diabetico :: Bool, hipertenso :: Bool.

*/
iniciaPaciente :- dynamic paciente/11.

/*

Inicializa a tabela de UBS.
Os campos são: id :: Int, nome :: String, endereco :: String.

*/
iniciaUBS :- dynamic ubs/3.

/*

Inicializa a tabela de médico e as tabelas auxiliares dos horários.
Os campos de médico são: id :: Int, nome :: String, crm :: String,
idUbs :: Int, especialidade :: String.

Os campos de horário (m_horarios) são: idMed :: Int, horario :: date(Y, M, D, H, MN, 0.0, 10800, -, -).
As datas sempre tem 0.0 segundos, e o 10800 indica que o fuso é GMT-3, ou seja, horário de Brasilia.

Os campos de horário de início e fim (m_inicio, m_fim) são idMed :: Int, horario :: time(H, M),
dia da semana :: Int (1 a 7).

Os campos de tempo de atendimento (m_tempo) são idMed :: Int, tempo de consulta em minutos :: Int.

*/
iniciaMedico :- dynamic(medico/5).

iniciaMInicio :- dynamic(m_inicio/3).

iniciaMFim :- dynamic(m_fim/3).

iniciaMTempo :- dynamic(m_tempo/2).

iniciaMHorarios :- dynamic(m_horarios/2).

/*

Inicializa a tabela de receita e a tabela auxiliar dos remédios de uma receita.
Os campos de receita são: id :: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int.

Os campos de remédio são: idReceita :: Int, idMedicamento :: Int, instruções :: String,
quantidade :: Int.

*/
iniciaReceita :- dynamic(receita/4),
    dynamic(receita_remedio/4).

/*

Inicializa a tabela de medicamentos.
Os campos são: id :: Int, idUBS :: Int, nome :: String, qtdEstoque :: Int, bula :: String.

*/
iniciaMedicamento :- dynamic medicamento/5.

/*

Inicializa a tabela de laudos.
Os campos são: id :: Int, idMed :: Int, idExame :: Int, texto :: String.

*/
iniciaLaudo :- dynamic laudo/4.

/*

Inicializa a tabela de consultas.
Os campos são: id:: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int, dia :: String

*/
iniciaConsulta :- dynamic consulta/5.

/*

Inicializa a tabela de exame.
Os campos são: id :: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int,
tipo :: String, dia :: TBD, resultado :: String.

*/
iniciaExame :- dynamic exame/7.

/*

Inicializa a tabela de logins.
Os campos são: id :: Int, senha :: String, tipoUsuário :: Int.

Tipo de usuário: 0 - Paciente, 1 - UBS, 2 - Médico.

*/
iniciaLogins :- dynamic logins/3.

/*

Inicializa os ids do sistema, contando do 0.

*/
iniciaId :- asserta(id(0)).

/*

Pega o próximo ID do sistema.
nextId(+N).

*/
nextId(N) :- id(X), retract(id(X)), N is X + 1, asserta(id(N)).


/*

Inicializa todas as tabelas do sistema de uma só vez.

*/
iniciaSistema :- verificaPaciente, verificaMedico, verificaUBS, verificaReceita, verificaMedicamento,
                 verificaLaudo, verificaExame, verificaLogins, verificaConsulta, verificaId,
                 verificaHorario, verificaMInicio, verificaMFim, verificaMTempo.

verificaPaciente :- exists_file('bd/paciente.bd') -> lePaciente ; iniciaPaciente.

verificaMedico :- exists_file('bd/medico.bd') -> leMedico ; iniciaMedico.

verificaUBS :- exists_file('bd/ubs.bd') -> leUBS ; iniciaUBS.

verificaReceita :- exists_file('bd/receita.bd') -> leReceita ; iniciaReceita.

verificaMedicamento :- exists_file('bd/medicamento.bd') -> leMedicamento ; iniciaMedicamento.

verificaLaudo :- exists_file('bd/laudo.bd') -> leLaudo ; iniciaLaudo.

verificaExame :- exists_file('bd/exame.bd') -> leExame ; iniciaExame.

verificaLogins :- exists_file('bd/logins.bd') -> leLogins ; iniciaLogins.

verificaConsulta :- exists_file('bd/consulta.bd') -> leConsulta ; iniciaConsulta.

verificaId :- exists_file('bd/id.bd') -> leId ; iniciaId.

verificaHorario :- exists_file('bd/medico_horarios.bd') -> leHorarios ; iniciaMHorarios.

verificaMInicio :- exists_file('bd/medico_inicio.bd') -> leMInicio ; iniciaMInicio.

verificaMFim :- exists_file('bd/medico_fim.bd') -> leMFim ; iniciaMFim.

verificaMTempo :- exists_file('bd/medico_tempo.bd') -> leMTempo ; iniciaMTempo.

/*
Verifica se o arquivo 'paciente.bd' existe, se existir o lê,
c.c. chama model:iniciaPaciente.
*/
lePaciente :- consult('bd/paciente.bd').


/*
Verifica se o arquivo 'ubs.bd' existe, se existir o lê,
c.c. chama model:iniciaUBS.
*/
leUBS :- consult('bd/ubs.bd').

/*
Verifica se o arquivo 'medico.bd' existe, se existir o lê,
c.c. chama model:iniciaMedico.
*/
leMedico :- consult('bd/medico.bd').

/*
Verifica se o arquivo 'paciente.bd' existe, se existir o lê,
c.c. chama model:iniciaPaciente.
*/
leConsulta :- consult('bd/consulta.bd').

/*
Verifica se o arquivo 'receita.bd' existe, se existir o lê,
c.c. chama model:iniciaReceita.
*/
leReceita :- consult('bd/receita.bd').

/*
Verifica se o arquivo 'medicamento.bd' existe, se existir o lê,
c.c. chama model:iniciaMedicamento.
*/
leMedicamento :- consult('bd/medicamento.bd').

/*
Verifica se o arquivo 'laudo.bd' existe, se existir o lê,
c.c. chama model:iniciaLaudo.
*/
leLaudo :- consult('bd/laudo.bd').

/*
Verifica se o arquivo 'exame.bd' existe, se existir o lê,
c.c. chama model:iniciaExame.
*/
leExame :- consult('bd/exame.bd').

/*
Verifica se o arquivo 'logins.bd' existe, se existir o lê,
c.c. chama model:iniciaLogin.
*/
leLogins :- consult('bd/logins.bd').

/*
Verifica se o arquivo 'id.bd' existe, se existir o lê,
c.c. chama model:iniciaId.
*/
leId :- consult('bd/id.bd').

leHorarios :- consult('bd/medico_horarios.bd').

/* Persiste a tabela medico_inicio no arquivo. */
leMInicio :- consult('bd/medico_inicio.bd').

/* Persiste a tabela medico_fim no arquivo. */
leMFim :- consult('bd/medico_fim.bd').

/* Persiste a tabela medico_fim no arquivo. */
leMTempo :- consult('bd/medico_tempo.bd').