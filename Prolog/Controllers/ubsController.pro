:- module(ubs, [validaIDMedicamento/1, 
                validaIDExame/1, validaIDReceita/1, validaIDLaudo/1, validaIDConsulta/1,
                visualizaConsultasFuturas/5, visualizaPacientes/12, visualizaMedicos/5,
                visualizaMedico/5, consultarMedicamentos/5, consultarMedicamento/5,
                adicionaMedicamentoEstoque/5, removeMedicamentoEstoque/5]).


/* Cria um médico. */
cadastraMedico.

/* Visualiza as consultas agendadas na UBS para hoje ou posteriori.
    visualizaConsultasFuturas(?IdUBS, -IdConsulta, -IdPaciente, -IdMedico, -Dia)
*/
visualizaConsultasFuturas(IdUBS, IdConsulta, IdPaciente, IdMedico, Dia).

/* Visualiza as informações dos pacientes com consultas agendadas. 
    visualizaPacientes(?IdUBS, -IdPaciente, -Nome, -Endereco, -CPF, -Dia, -Peso, -Altura, -TipoSanguineo, -C, -D, -H)
*/
visualizaPacientes(IdUBS, IdPaciente, Nome, Endereco, CPF, Dia, Peso, Altura, TipoSanguineo, C, D, H).

/* Visualiza os médicos que trabalham na UBS. 
    visualizaMedicos(-IdMed, -Nome, -CRM, ?IdUBS, -Especialidade)
*/
visualizaMedicos(IdMed, Nome, CRM, IdUBS, Especialidade).

/* Visualiza os médico que trabalham na UBS.
    visualizaMedico(?IdMed, -Nome, -CRM, ?IdUBS, -Especialidade)
*/
visualizaMedico(IdMed, Nome, CRM, IdUBS, Especialidade).

/* Adiciona uma quantidade no estoque de um medicamento.
    adicionaMedicamentoEstoque(?IdMed, ?IdUBS, -Nome, ?Qtd, -Bula)
*/
adicionaMedicamentoEstoque(IdMed, IdUBS, Nome, Qtd, Bula).

/* Retira uma quantiade do estoque de um medicamento. 
    removeMedicamentoEstoque(?IdMed, ?IdUBS, -Nome, ?Qtd, -Bula)
*/
removeMedicamentoEstoque(IdMed, IdUBS, Nome, Qtd, Bula).

/* Visualiza informações de todos os medicamentos da UBS. 
    consultarMedicamentos(-IdMed, ?IdUBS, -Nome, -Qtd, -Bula)
*/
consultarMedicamentos(IdMed, IdUBS, Nome, Qtd, Bula).

/* Visualiza informações de um medicamento específico da UBS. 
    consultarMedicamentos(?IdMed, ?IdUBS, -Nome, -Qtd, -Bula)
*/
consultarMedicamento(IdMed, IdUBS, Nome, Qtd, Bula).

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


/* 
Verifica se o ID pertence a uma consulta. 

@param ID: id da Consulta.
*/
validaIDConsulta(ID).