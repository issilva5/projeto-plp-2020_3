:- module(medico, [validaIDMedico/1, acessarExames/2, 
    acessarDadosPaciente/1, acessarConsultas/2, 
    acessarMedicamentos/0, acessarMedicamentos/1, 
    solicitarTransferencia/2, emitirReceita/2,
    emitirResultadoExame/1, emitirLaudo/2]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/time.pro').
:- use_module('./Persistence/persistence.pro').
:- use_module('../Controllers/ubsController.pro', [validaIDMedicamento/1]).

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
           show:showExame(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))). % ; true

acessarExamesPac(IdPac) :-
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
           show:showExame(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))).

acessarExame(IdEx) :-
    forall(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado),
            show:showExame(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado))).


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
    time:string_to_date(DataString, date(A, M, D, _, _, _, _, _, _)),
    forall(model:consulta(ID, IdPac, IDM, IdUBS, date(A, M, D, H, MN, S, O, -, -)),
    show:showConsulta(model:consulta(ID, IdPac, IDM, IdUBS, date(A, M, D, H, MN, S, O, -, -)))).


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

/* 

Cria uma receita.
@param +IDM: id do médico
@param +IdPac: id do paciente

*/
emitirReceita(IDM, IdPac) :- model:nextId(Id),
    pegaUBS(IDM, IdUBS),
    persistence:escreveId,
    assertz(model:receita(Id, IdPac, IDM, IdUBS)),
    persistence:escreveReceita,
    lerRemedios(Id).

lerRemedios(IdRec) :-
    utils:prompt('Insira o ID do remédio > ', IdMedic),
    utils:promptString('Insira as instruções de uso > ', Inst),
    utils:prompt('Insira a quantidade de caixas > ', Q),
    (ubs:validaIDMedicamento(IdMedic), ! ; write('ID inválido\n\n'), lerRemedios(IdRec)),
    assertz(model:receita_remedio(IdRec, IdMedic, Inst, Q)),
    persistence:escreveReceitaRem,
    utils:promptString('Deseja inserir outro medicamento (s/n) > ', O),
    (O = "s" -> lerRemedios(IdRec) ; true).


/* Adiciona o resultado de um exame. */
emitirResultadoExame(IdExame) :- utils:promptString('Insira o resultado > ', Resultado),
    model:exame(IdExame, IdPac, IDM, IdUBS, Tipo, Data, R),
    retract(model:exame(IdExame, IdPac, IDM, IdUBS, Tipo, Data, R)), !,
    assertz(model:exame(IdExame, IdPac, IDM, IdUBS, Tipo, Data, Resultado)),
    persistence:escreveExame.

/* Cria um laudo. */
emitirLaudo(IdMed, IdExame) :- model:nextId(Id),
    persistence:escreveId,
    utils:promptString('Insira o texto do laudo > ', Texto),
    assertz(model:laudo(Id, IdMed, IdExame, Texto)),
    persistence:escreveLaudo.

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
pegaUBS(IDM, IdUBS) :- model:medico(IDM, _, _, IdUBS, _).

/* 
Verifica se o id pertence a um médico. 

@param ID: id do médico.
*/
validaIDMedico(ID) :- model:medico(ID, _, _, _, _).