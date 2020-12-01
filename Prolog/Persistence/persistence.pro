:- module(persistence, []).
:- use_module('../Models/model.pro').

/*
Verifica se o arquivo 'paciente.bd' existe, se existir o lê,
c.c. chama model:iniciaPaciente.
*/
lePaciente :- exists_file('paciente.bd'), consult('paciente.bd').


/*
Verifica se o arquivo 'ubs.bd' existe, se existir o lê,
c.c. chama model:iniciaUBS.
*/
leUBS.

/*
Verifica se o arquivo 'medico.bd' existe, se existir o lê,
c.c. chama model:iniciaMedico.
*/
leMedico.

/*
Verifica se o arquivo 'receita.bd' existe, se existir o lê,
c.c. chama model:iniciaReceita.
*/
leReceita.

/*
Verifica se o arquivo 'medicamento.bd' existe, se existir o lê,
c.c. chama model:iniciaMedicamento.
*/
leMedicamento.

/*
Verifica se o arquivo 'laudo.bd' existe, se existir o lê,
c.c. chama model:iniciaLaudo.
*/
leLaudo.

/*
Verifica se o arquivo 'exame.bd' existe, se existir o lê,
c.c. chama model:iniciaExame.
*/
leExame.

/*
Verifica se o arquivo 'logins.bd' existe, se existir o lê,
c.c. chama model:iniciaLogin.
*/
leLogins.

/*
Verifica se o arquivo 'id.bd' existe, se existir o lê,
c.c. chama model:iniciaId.
*/
leId.

/* Persiste a tabela paciente no arquivo 'paciente.bd'. */
escrevePaciente :- tell('paciente.bd'), listing(model:paciente), told.

/* Persiste a tabela UBS no arquivo 'ubs.bd'. */
escreveUBS.

/* Persiste as tabelas medico e medico_horarios no arquivo 'medico.bd'. */
escreveMedico.

/* Persiste a tabela receita no arquivo 'receita.bd'. */
escreveReceita.

/* Persiste a tabela medicamento no arquivo 'medicamento.bd'. */
escreveMedicamento.

/* Persiste a tabela laudo no arquivo 'laudo.bd'. */
escreveLaudo.

/* Persiste a tabela exame no arquivo 'exame.bd'. */
escreveExame.

/* Persiste a tabela logins no arquivo 'logins.bd'. */
escreveLogins.

/* Persiste a tabela id no arquivo 'id.bd'. */
escreveId.