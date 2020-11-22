:- module(ubs, [validaIDUBS/2, validaIDMedicamento/1, 
                validaIDExame/1, validaIDReceita/1, validaIDLaudo/1]).

/* Cria um médico. */
cadastraMedico.

/* Visualiza as consultas agendadas na UBS para hoje ou posteriori. */
visualizaConsultasFuturas.

/* Visualiza todo o histórico de consultas agendadas. */
visualizaConsultas.

/* Visualiza as informações dos pacientes com consultas agendadas. */
visualizaPacientes.

/* Visualiza os médicos que trabalham na UBS. */
visualizaMedicos.

/* Cria um medicamento. */
criaMedicamento.

/* Adiciona uma quantidade no estoque de um medicamento. */
adicionaMedicamentoEstoque.

/* Retira uma quantiade do estoque de um medicamento. */
removeMedicamentoEstoque.

/* Visualiza informações de todos os medicamentos da UBS. */
consultarMedicamentos.

/* Visualiza informações de um medicamento específico da UBS. */
consultarMedicamento.

/*

Determina o status dos médicos da UBS. 

Um médico pode estar:
0 - sem plantão,
1 - em plantão e sem consulta,
2 - em plantão e em consulta.

*/
statusMedico.

/* 
Verifica se o ID pertence a uma UBS. 

@param ID: id da ubs.
*/
validaIDUBS(ID).

/* 
Verifica se o ID pertence a um medicamento. 

@param ID: id do medicamento.
*/
validaIDMedicamento(ID).

/* 
Verifica se o ID pertence a um exame. 

@param ID: id do exame.
*/
validaIDExame(ID).

/* 
Verifica se o ID pertence a uma receita. 

@param ID: id da receita.
*/
validaIDReceita(ID).

/* 
Verifica se o ID pertence a um laudo. 

@param ID: id do laudo.
*/
validaIDLaudo(ID).