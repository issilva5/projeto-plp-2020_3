:- module(medico, [validaIDMedico/1, acessarExames/2, acessarDadosPaciente/1, acessarConsultas/2, acessarMedicamentos/0, acessarMedicamentos/1, solicitarTransferencia/2]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/time.pro').

/*

Imprime os dados de um paciente a partir de seu ID.
@param +IdPac: id do paciente.

*/
acessarDadosPaciente(IdPac) :- 
    forall(model:paciente(IdPac, Nome, CPF, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper),
           show:showPaciente(model:paciente(IdPac, Nome, CPF, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper))).

/*

Acessar exames pelo id do médico, do paciente ou do exame.
@param +ID: id a ser buscado.
@param +O: tipo do ID, 0 para médico, 1 para paciente e qualquer outro para Exame. 

*/
acessarExames(ID, O) :- (O =:= 0 -> acessarExamesMed(ID) ; O =:= 1 -> acessarExame(ID) ; acessarExamesPac(ID)).

acessarExamesMed(IDM) :-
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
           show:showPaciente(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))). % ; true

acessarExamesPac(IdPac) :-
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
           show:showPaciente(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))).

acessarExame(IdEx) :-
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
            show:showPaciente(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))).


/* 

Acessa as consultas do médico. 
@param +IDM: id do médico
@param +O: se 0 acessa todas as consultas, se for uma data consulta por data.

*/
acessarConsultas(IDM, O) :- (O =:= 0 -> acessarConsultasMed(IDM) ; acessarConsultasMed(IDM, O)).

acessarConsultasMed(IDM) :-
    forall(model:consulta(ID, IdPac, IDM, IdUBS, Data),
    show:showConsulta(model:consulta(ID, IdPac, IDM, IdUBS, Data))).

acessarConsultasMed(IDM, DataString) :- 
    time:string_to_date(DataString, Data), %TODO
    forall(model:consulta(ID, IdPac, IDM, IdUBS, Data),
    show:showConsulta(model:consulta(ID, IdPac, IDM, IdUBS, Data))).


/*

Acessa todos os medicamentos.

*/
acessarMedicamentos :-
    forall(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula),
           show:showMedicamento(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula))).

/*

Acessa um medicamento específico.
@param +IdMed: id do medicamento

*/
acessarMedicamentos(IdMed) :-
    forall(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula),
            show:showMedicamento(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula))).

/* Cria uma receita. */
emitirReceita.

/* Adiciona o resultado de um exame. */
emitirResultadoExame.

/* Cria um laudo. */
emitirLaudo.

/*

Altera a UBS do médico.
@param +IDM: id do médico
@param +IdNovaUBS: id da nova UBS.

TODO persistência

*/
solicitarTransferencia(IDM, IdNovaUBS) :- 
    model:medico(IDM, Nome, CRM, IdUBS, Espec),
    retract(model:medico(IDM, Nome, CRM, IdUBS, Espec)),
    assertz(model:medico(IDM, Nome, CRM, IdNovaUBS, Espec)).

/* Consulta o id da UBS de um médico. */
pegaUBS.

/* 
Verifica se o id pertence a um médico. 

@param ID: id do médico.
*/
validaIDMedico(ID).