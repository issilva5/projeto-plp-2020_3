:- module(medico, [validaIDMedico/1, acessarExames/1]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').

/* Acessar dados de um paciente. */
acessarDadosPaciente(IdPac) :- 
    forall(model:paciente(IdPac, Nome, CPF, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper),
           show:showPaciente(model:paciente(IdPac, Nome, CPF, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper))).

/* Acessar exames do médico. */
acessarExames(IDM) :- 
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
           show:showPaciente(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))).


/* Acessar consultas do médico. */
acessarConsultas.

/* Acessar consultas de uma determinada data. */
acessarConsultasData.

/* Acessar consulta específica de um médico. */
acessarConsulta.

/* Cria uma receita. */
emitirReceita.

/* Adiciona o resultado de um exame. */
emitirResultadoExame.

/* Cria um laudo. */
emitirLaudo.

/* Acessar um exame específico do médico. */
acessarExame.

/* Acessar exames de um paciente. */
acessarExamePaciente.

/* Muda a UBS do médico. */
solicitarTransferencia.

/* Consulta o id da UBS de um médico. */
pegaUBS.

/* 
Verifica se o id pertence a um médico. 

@param ID: id do médico.
*/
validaIDMedico(ID).