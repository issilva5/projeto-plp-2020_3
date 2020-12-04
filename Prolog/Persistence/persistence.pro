:- module(persistence, [escrevePaciente/0, escreveUBS/0,
                        escreveMedico/0, escreveReceita/0, escreveReceitaRem/0, escreveMedicamento/0, escreveLaudo/0,
                        escreveConsulta/0, escreveExame/0, escreveLogins/0, escreveId/0,
                        escreveHorarios/0, escreveMInicio/0, escreveMFim/0]).

:- use_module('../Models/model.pro').

/*
lePaciente/0, leUBS/0, leMedico/0, leMedico/0, leReceita/0, leMedicamento/0,
                        leLaudo/0, leExame/0, leLogins/0, leConsulta/0, leId/0, 

*/

/* Persiste a tabela paciente no arquivo 'paciente.bd'. */
escrevePaciente :- tell('bd/paciente.bd'), listing(model:paciente), told.

/* Persiste a tabela UBS no arquivo 'ubs.bd'. */
escreveUBS :- tell('bd/ubs.bd'), listing(model:ubs), told.

/* Persiste as tabelas medico e medico_horarios no arquivo 'medico.bd'. */
escreveMedico :- tell('bd/medico.bd'), listing(model:medico), told.

/* Persiste a tabela receita no arquivo 'receita.bd'. */
escreveReceita :- tell('bd/receita.bd'), listing(model:receita), told.

/* Persiste a tabela receita no arquivo 'receita.bd'. */
escreveReceitaRem :- tell('bd/receita_rem.bd'), listing(model:receita_remedio), told.

/* Persiste a tabela medicamento no arquivo 'medicamento.bd'. */
escreveMedicamento :- tell('bd/medicamento.bd'), listing(model:medicamento), told.

/* Persiste a tabela laudo no arquivo 'laudo.bd'. */
escreveLaudo :- tell('bd/laudo.bd'), listing(model:laudo), told.

/* Persiste a tabela consulta no arquivo 'consulta.bd'. */
escreveConsulta :- tell('bd/consulta.bd'), listing(model:consulta), told.

/* Persiste a tabela exame no arquivo 'exame.bd'. */
escreveExame :- tell('bd/exame.bd'), listing(model:exame), told.

/* Persiste a tabela logins no arquivo 'logins.bd'. */
escreveLogins :- tell('bd/logins.bd'), listing(model:logins), told.

/* Persiste a tabela id no arquivo 'id.bd'. */
escreveId :- tell('bd/id.bd'), listing(model:id), told.

/* Persiste a tabela medico_horarios no arquivo. */
escreveHorarios :- tell('bd/medico_horarios.bd'), listing(model:m_horarios), told.

/* Persiste a tabela medico_inicio no arquivo. */
escreveMInicio :- tell('bd/medico_inicio.bd'), listing(model:m_inicio), told.

/* Persiste a tabela medico_fim no arquivo. */
escreveMFim :- tell('bd/medico_fim.bd'), listing(model:m_fim), told.

/* Persiste a tabela medico_fim no arquivo. */
escreveMTempo :- tell('bd/medico_tempo.bd'), listing(model:m_tempo), told.