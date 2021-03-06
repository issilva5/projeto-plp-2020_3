:- module(ubs, [validaIDMedicamento/1,
                validaIDExame/1, validaIDReceita/1, validaIDLaudo/1,
                visualizaConsultasFuturas/1, visualizaPacientes/1, visualizaMedicos/1,
                visualizaMedico/2, consultarMedicamentos/1, consultarMedicamento/2,
                adicionaMedicamentoEstoque/3, removeMedicamentoEstoque/3,
                consultasHoje/1, estoqueEmBaixa/1]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/utils.pro').
:- use_module('../Utils/time.pro', [getStatusMedico/2]).
:- use_module('../Persistence/persistence.pro').

/* Cria um médico. */
cadastraMedico(IdUBS) :-
    promptString('Nome > ', Nome),
    promptString('CRM > ', CRM),
    promptString('Especialidade > ', Especialidade),
    promptPassword('Senha > ', Senha),
    model:nextId(N),
    assertz(model:medico(N, Nome, CRM, IdUBS, Especialidade)),
    assertz(model:logins(N, Senha, 2)),
    persistence:escreveId,
    persistence:escreveMedico,
    persistence:escreveLogins,
    format('\nCadastrado de médico realizado com sucesso, id: ~d', [N]).

/* Visualiza as consultas agendadas na UBS para hoje ou posteriori.
    visualizaConsultasFuturas(?IdUBS, -IdConsulta, -IdPaciente, -IdMedico, -Dia)
*/
visualizaConsultasFuturas(IdUBS) :-
    forall((model:consulta(IdConsulta, IdPac, IdMed, IdUBS, Data),
            get_time(X), stamp_date_time(X, Today, 10800),
            Data @>= Today),
        (show:showConsulta(model:consulta(IdConsulta, IdPac, IdMed, IdUBS, Data)))).

/* Visualiza as informações dos pacientes com consultas agendadas.
    visualizaPacientes(?IdUBS, -IdPaciente, -Nome, -Endereco, -CPF, -Dia, -Peso, -Altura, -TipoSanguineo, -C, -D, -H)
*/
visualizaPacientes(IdUBS) :-
    findall(IdPac, (model:consulta(_, IdPac, _, IdUBS, Data),
            get_time(X), stamp_date_time(X, Today, 10800),
            Data @>= Today), L),
    list_to_set(L, S),
    foreach(member(IdP, S), showP(IdP)).

showP(IdP) :- model:paciente(IdP, Nome, _, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper),
    show:showPaciente(model:paciente(IdP, Nome, _, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper)).

/* Visualiza os médicos que trabalham na UBS.
    visualizaMedicos(-IdMed, -Nome, -CRM, ?IdUBS, -Especialidade)
*/
visualizaMedicos(IdUBS) :-
    forall(model:medico(IdMed, Nome, CRM, IdUBS, Especialidade),
    show:showMedico(model:medico(IdMed, Nome, CRM, IdUBS, Especialidade))).

/* Visualiza os médico que trabalham na UBS.
    visualizaMedico(?IdMed, -Nome, -CRM, ?IdUBS, -Especialidade)
*/
visualizaMedico(IdMed, IdUBS) :-
    model:medico(IdMed, Nome, CRM, IdUBS, Especialidade),
    show:showMedico(model:medico(IdMed, Nome, CRM, IdUBS, Especialidade)).

/* Adiciona uma quantidade no estoque de um medicamento.
    adicionaMedicamentoEstoque(?IdMed, ?IdUBS, -Nome, ?Qtd, -Bula)
*/
adicionaMedicamentoEstoque(IdMed, IdUBS, Qtd) :-
    model:medicamento(IdMed, IdUBS, Nome, QtdAtual, Bula),
    QuantidadeAtualizada is QtdAtual + Qtd,
    retract(model:medicamento(IdMed, IdUbs, Nome, QtdAtual, Bula)),
    assertz(model:medicamento(IdMed, IdUbs, Nome, QuantidadeAtualizada, Bula)),
    persistence:escreveMedicamento,
    show:showMedicamento(model:medicamento(IdMed, IdUbs, Nome, QuantidadeAtualizada, Bula)).

/* Retira uma quantiade do estoque de um medicamento.
    removeMedicamentoEstoque(?IdMed, ?IdUBS, -Nome, ?Qtd, -Bula)
*/
removeMedicamentoEstoque(IdMed, IdUbs, Qtd) :-
    adicionaMedicamentoEstoque(IdMed, IdUbs, (Qtd * -1)).

/* Visualiza informações de todos os medicamentos da UBS.
    consultarMedicamentos(-IdMed, ?IdUBS, -Nome, -Qtd, -Bula)
*/
consultarMedicamentos(IdUBS) :-
    forall(model:medicamento(IdMed, IdUBS, Nome, Qtd, Bula),
    show:showMedicamento(model:medicamento(IdMed, IdUBS, Nome, Qtd, Bula))).

/* Visualiza informações de um medicamento específico da UBS.
    consultarMedicamentos(?IdMed, ?IdUBS, -Nome, -Qtd, -Bula)
*/
consultarMedicamento(IdMed, IdUbs) :-
    model:medicamento(IdMed, IdUbs, Nome, Qtd, Bula),
    show:showMedicamento(model:medicamento(IdMed, IdUbs, Nome, Qtd, Bula)).

/*

Determina o status dos médicos da UBS.

Um médico pode estar:
0 - sem plantão,
1 - em plantão e sem consulta,
2 - em plantão e em consulta.
*/
statusMedico(IdUbs) :-
    format('Status dos médicos:~n'),
    forall(model:medico(IdMed, Nome, _, IdUbs, _), 
    (time:getStatusMedico(IdMed, Status),
     format('Médico ~w de id ~d está ~w.~n', [Nome, IdMed, Status]))).

consultasHoje(IdUbs) :-
    format('Consultas do Dia:~n'),
    forall((model:consulta(IdConsulta, IdPac, IdMed, IdUbs, DataC),
            get_time(X), stamp_date_time(X, TodayC, 10800),
            date_time_value(date, DataC, Data),
            date_time_value(date, TodayC, Today),
            Data =@= Today,
            date_time_value(time, DataC, time(H, M, _))),
        format('Consulta ~d do paciente ~d com o doutor ~d às ~d:~d~n', [IdConsulta, IdPac, IdMed, H, M])).

estoqueEmBaixa(IdUbs) :-
    format('Medicamentos com estoque em baixa:~n'),
    forall((model:medicamento(_, IdUbs, Nome, Qtd, _),
            Qtd < 15),
        format('~w - ~d~n', [Nome, Qtd])).


/*
Verifica se o ID pertence a uma UBS.
@param ID: id da ubs.
*/
validaIDUBS(ID) :- model:ubs(ID, _, _).

/*
Verifica se o ID pertence a um medicamento.
@param ID: id do medicamento.
*/
validaIDMedicamento(ID) :- model:medicamento(ID, _, _, _, _).

/*
Verifica se o ID pertence a um exame.
@param ID: id do exame.
*/
validaIDExame(ID) :- model:exame(ID, _, _, _, _, _, _).

/*
Verifica se o ID pertence a uma receita.
@param ID: id da receita.
*/
validaIDReceita(ID) :- model:receita(ID, _, _, _).

/*
Verifica se o ID pertence a um laudo.
@param ID: id do laudo.
*/
validaIDLaudo(ID) :- model:laudo(ID, _, _, _).

/* 
Verifica se o ID pertence a uma consulta. 
@param ID: id da Consulta.
*/
validaIDConsulta(ID) :- model:consulta(ID, _, _, _, _).
