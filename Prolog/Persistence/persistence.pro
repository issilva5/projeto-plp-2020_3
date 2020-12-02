:- module(persistence, []).
:- use_module('../Models/model.pro').

/*
Verifica se o arquivo 'paciente.bd' existe, se existir o lê,
c.c. chama model:iniciaPaciente.
*/
lePaciente :- consult('paciente.bd').


/*
Verifica se o arquivo 'ubs.bd' existe, se existir o lê,
c.c. chama model:iniciaUBS.
*/
leUBS :- consult('ubs.bd').

/*
Verifica se o arquivo 'medico.bd' existe, se existir o lê,
c.c. chama model:iniciaMedico.
*/
leMedico :- consult('medico.bd').

/*
Verifica se o arquivo 'receita.bd' existe, se existir o lê,
c.c. chama model:iniciaReceita.
*/
leReceita :- consult('receita.bd').

/*
Verifica se o arquivo 'medicamento.bd' existe, se existir o lê,
c.c. chama model:iniciaMedicamento.
*/
leMedicamento :- consult('medicamento.bd').

/*
Verifica se o arquivo 'laudo.bd' existe, se existir o lê,
c.c. chama model:iniciaLaudo.
*/
leLaudo :- consult('laudo.bd').

/*
Verifica se o arquivo 'exame.bd' existe, se existir o lê,
c.c. chama model:iniciaExame.
*/
leExame :- consult('exame.bd').

/*
Verifica se o arquivo 'logins.bd' existe, se existir o lê,
c.c. chama model:iniciaLogin.
*/
leLogins :- consult('logins.bd').

/*
Verifica se o arquivo 'id.bd' existe, se existir o lê,
c.c. chama model:iniciaId.
*/
leId :- consult('id.bd').

/* Persiste a tabela paciente no arquivo 'paciente.bd'. */
escrevePaciente :- tell('paciente.bd'), listing(model:paciente), told.

/* Persiste a tabela UBS no arquivo 'ubs.bd'. */
escreveUBS :- tell('ubs.bd'), listing(model:ubs), told.

/* Persiste as tabelas medico e medico_horarios no arquivo 'medico.bd'. */
escreveMedico :- tell('medico.bd'), listing(model:medico), told.

/* Persiste a tabela receita no arquivo 'receita.bd'. */
escreveReceita :- tell('receita.bd'), listing(model:receita), told.

/* Persiste a tabela medicamento no arquivo 'medicamento.bd'. */
escreveMedicamento :- tell('medicamento.bd'), listing(model:medicamento), told.

/* Persiste a tabela laudo no arquivo 'laudo.bd'. */
escreveLaudo :- tell('laudo.bd'), listing(model:laudo), told.

/* Persiste a tabela exame no arquivo 'exame.bd'. */
escreveExame :- tell('exame.bd'), listing(model:exame), told.

/* Persiste a tabela logins no arquivo 'logins.bd'. */
escreveLogins :- tell('logins.bd'), listing(model:logins), told.

/* Persiste a tabela id no arquivo 'id.bd'. */
escreveId :- tell('id.bd'), listing(model:id), told.