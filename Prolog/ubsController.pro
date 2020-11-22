:- module(ubs, [visualizaConsultasFuturas/1, visualizaPacientes/1, visualizaMedicos/1, visualizaMedico/2, consultarMedicamentos/1, consultarMedicamento/2, validaIDMedicamento/2, adicionaMedicamentoEstoque/3, removeMedicamentoEstoque/3]).

/* Visualiza as consultas agendadas na UBS para hoje ou posteriori. */
visualizaConsultasFuturas(_).

/* Visualiza as informações dos pacientes com consultas agendadas. */
visualizaPacientes(_).

/* Visualiza os médicos que trabalham na UBS. */
visualizaMedicos(_).

/* Visualiza os médico que trabalham na UBS. */
visualizaMedico(_, _).

/* Adiciona uma quantidade no estoque de um medicamento. */
adicionaMedicamentoEstoque(_, _, _).

/* Retira uma quantiade do estoque de um medicamento. */
removeMedicamentoEstoque(_, _, _).

/* Visualiza informações de todos os medicamentos da UBS. */
consultarMedicamentos(_).

/* Visualiza informações de um medicamento específico da UBS. */
consultarMedicamento(_, _).

/*

Determina o status dos médicos da UBS. 

Um médico pode estar:
0 - sem plantão,
1 - em plantão e sem consulta,
2 - em plantão e em consulta.

*/
statusMedico.

/* Verifica se o ID pertence a uma UBS. */
validaIDUBS.

/* Verifica se o ID pertence a um medicamento. */
validaIDMedicamento(_, _).

/* Verifica se o ID pertence a um exame. */
validaIDExame.

/* Verifica se o ID pertence a uma receita. */
validaIDReceita.

/* Verifica se o ID pertence a um laudo. */
validaIDLaudo.