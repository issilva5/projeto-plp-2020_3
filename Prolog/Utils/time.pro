:- module(time, [medicoHorarios/1, getNextDate/2, string_to_date/2]).

:- use_module('utils.pro').
:- use_module('../Models/model.pro').
:- use_module('../Persistence/persistence.pro').

/*

Faz o rolê acontecer. ¯\_(ツ)_/¯

*/
medicoHorarios(Id) :- informaHorarios(Id), iniciaDatas(Id).

/*

Obtem o próximo horário livre de um médico.
@param +Id: Id do médico
@param -D: próximo horário livre

*/
getNextDate(Id, D) :- model:m_horarios(Id, D), retract(model:m_horarios(Id, D)), !, putNextDate(Id, D).

/*

Salva no BD a próxima data do médico.
@param +Id: Id do médico
@param +D: última data retirada.

É chamado, internamente, após o getNextDate.

@see getNextDate.

*/
putNextDate(Id, D) :- model:m_tempo(Id, T),
    add_minutes(D, T, NewDate),
    date_time_value(date, D, OnlyDate),
    day_of_the_week(OnlyDate, DayOfTheWeek),
    model:m_fim(Id, TF, DayOfTheWeek),
    combine_dt(OnlyDate, TF, MaxDate),
    compare_dates(NewDate, MaxDate, C),
    (C =:= -1 -> asserta(model:m_horarios(Id, NewDate)) ; 
        model:m_inicio(Id, TI, DayOfTheWeek),
        add_days(NewDate, 7, OutputDate),
        date_time_value(date, OutputDate, OnlyDate2),
        combine_dt(OnlyDate2, TI, NewDate2),
        assertz(model:m_horarios(Id, NewDate2))), persistence:escreveHorarios, !.


/*

Inicia a tabela de horários de consulta de um médico.
@param +Id: Id do médico.

*/
iniciaDatas(Id) :- forall(
    (today(T),
    model:m_inicio(Id, time(H, M), W),
    next_weekday(T, W, D),
    combine_dt(D, time(H, M), O),
    today_weekday(Tw)),
    assertz(model:m_horarios(Id, O))), persistence:escreveHorarios.

/*

Faz a coleta dos horários de início e fim de plantão do médico.
@param +Id: Id do médico.

O horário informado deve ser no formato HH:MM, com H entre 0 e 23, e M entre 0 e 59.
Para pular um horário apenas aperte ENTER, ou digite qualquer coisa inválida.
Caso o horário de início ou de fim seja inválido, ambos serão considerados inválidos e desconsiderados.

*/
informaHorarios(Id) :- limpaTudo(Id), !, tempoConsulta(Id),
    persistence:escreveMTempo,
    today_weekday(W),
    Wd is W+1,
    forall(between(Wd,7,WeekD),
    (informaHorario(Id,WeekD) ; true)),
    forall(between(1,W,WeekD),
    (informaHorario(Id,WeekD) ; true)).

/*

Limpa informações anteriores sobre os horários do médico.
@param +Id: Id do médico.

*/
limpaTudo(Id) :- (retractall(model:m_horarios(Id, _)) ; true),
    (retractall(model:m_inicio(Id, _, _)) ; true),
    (retractall(model:m_fim(Id, _, _)) ; true),
    (retractall(model:m_tempo(Id, _)) ; true),
    persistence:escreveMInicio, persistence:escreveMFim,
    persistence:escreveMTempo, persistence:escreveHorarios.

/*

Faz a coleta do tempo de consulta do médico.
Permanece em loop até o valor que o valor lido seja maior que 0.
@param +Id: id do médico.

*/
tempoConsulta(Id) :- (prompt('Insira seu tempo médio de consulta > ', T), T > 0) -> asserta(model:m_tempo(Id, T)) ; tempoConsulta(Id).

/*

Faz a coleta dos horários de início e fim de plantão do médico para um dado dia da semana.
@param +Id: Id do médico.
@param +WeekD: dia da semana, onde 1 é Segunda-feira e 7 é Domingo.

@see informaHorarios

*/
informaHorario(Id, WeekD) :- informaHorarioInicio(Id, WeekD, MInicio),
    informaHorarioFim(Id, WeekD, MFim),
    assertz(MInicio),
    assertz(MFim),
    persistence:escreveMInicio,
    persistence:escreveMFim.

/*

Faz a coleta dos horários de início do médico para um dado dia da semana.
@param +Id: Id do médico.
@param +WeekD: dia da semana, onde 1 é Segunda-feira e 7 é Domingo.
@param -MInicio: átomo no formato m_inicio(Id, Hora, Minuto, DiaSemana)

@see informaHorario

*/
informaHorarioInicio(Id, WeekD, MInicio) :- weekday_name(WeekD, Name),
    format(atom(Question), 'Informe seu horário de início na ~w (HH:MM) > ', [Name]),
    utils:promptString(Question, HourString),
    split_string(HourString, ":", "", [HourS, MinuteS]),
    atom_number(HourS, Hour),
    atom_number(MinuteS, Minute),
    validaTempo(Hour, Minute),
    MInicio = model:m_inicio(Id, time(Hour, Minute), WeekD).

/*

Faz a coleta dos horários de início do médico para um dado dia da semana.
@param +Id: Id do médico.
@param +WeekD: dia da semana, onde 1 é Segunda-feira e 7 é Domingo.
@param -MFim: átomo no formato m_fim(Id, Hora, Minuto, DiaSemana)

@see informaHorario

*/
informaHorarioFim(Id, WeekD, MFim) :- weekday_name(WeekD, Name),
    format(atom(Question), 'Informe seu horário de fim na ~w (HH:MM) > ', [Name]),
    utils:promptString(Question, HourString),
    split_string(HourString, ":", "", [HourS, MinuteS]),
    atom_number(HourS, Hour),
    atom_number(MinuteS, Minute),
    validaTempo(Hour, Minute),
    MFim = model:m_fim(Id, time(Hour, Minute), WeekD).

/*

Verifica se um horário é válido.
@param +Hour: um inteiro que deve estar entre 0 e 23 para retornar true.
@param +Minute: um inteiro que deve estar entre 0 e 59 para retornar true.

*/
validaTempo(Hour, Minute) :- Hour >= 0, Hour < 24, Minute >= 0, Minute < 60.

/*

Adiciona uma quantidade de minutos a uma data.
@param +date(Y, M, D, H, MN, S, TZ, _, _): data no formato padrão da estrutura de data de Prolog
@param +Minutes: quantidade de minutos a ser adicionada
@param -DateOut: data com os minutos adicionados

*/
add_minutes(date(Y, M, D, H, MN, _, TZ, _, _), Minutes, DateOut) :-
    Aux is MN + Minutes,
    date_time_stamp(date(Y,M,D,H,Aux,0.0,TZ,-,-), Stamp),
    stamp_date_time(Stamp, DateOut, TZ).

/*

Adiciona uma quantidade de dias a uma data.
@param +Date: data no formato padrão da estrutura de data de Prolog - date(Y, M, D, H, MN, S, TZ, -, -)
@param +Minutes: quantidade de minutos a ser adicionada
@param -DateOut: data com os minutos adicionados

*/
add_days(Date, Days, DateOut) :-
    Minutes is Days * 24 * 60,
    add_minutes(Date, Minutes, DateOut).


/*

Pega a data do próximo dia da semana a partir de uma dada data.
@param +date(Y, M, D): data base
@param +WeekD: dia da semana
@param -DateOut: data do próximo dia da semana a partir da data base.

*/
next_weekday(_, WeekD, DateOut) :- (WeekD < 1 ; WeekD > 7), DateOut = "impossivel", !.

next_weekday(date(Y, M, D), WeekD, DateOut) :-
    add_days(date(Y, M, D, 0, 0, 0, 0, -, -), 1, DateAux1),
    date_time_value(date, DateAux1, DateAux2),
    day_of_the_week(DateAux2, DWeek),
    (WeekD =\= DWeek -> next_weekday(DateAux2, WeekD, DateOut) ; DateOut = DateAux2).

/*

Nomes dos dias da semana.

*/
weekday_name(1, "Segunda-feira").
weekday_name(2, "Terça-feira").
weekday_name(3, "Quarta-feira").
weekday_name(4, "Quinta-feira").
weekday_name(5, "Sexta-feira").
weekday_name(6, "Sábado").
weekday_name(7, "Domingo").

/*

Recupera a data do dia corrente, no horário oficial de Brasília.
@param -T: data de hoje

*/
today(T) :- get_time(X), stamp_date_time(X, D, 10800), date_time_value(date, D, T).

/*

Recupera o dia da semana do dia corrente, no horário oficial de Brasília.
@param -W: dia da semana de hoje

*/
today_weekday(W) :- today(T), day_of_the_week(T, W).

/*

Combina uma data no formato date(Y, M, D) e um horário no formato time(H, MN).
@param +date: data no formato date(Y, M, D)
@param +time: horário no formato time(H, MN)
@param -O: data no formato date(Y, M, D, H, MN, S, TZ, -, -), com segundos igual a 0 e TZ sendo GMT-3

*/
combine_dt(date(Y, M, D), time(H, MN), O) :- O = date(Y, M, D, H, MN, 0.0, 10800, -, -).

/*

Compara duas datas.
@param +D1: data 1
@param +D2: data 2
@param -C: 1 se data1 > data2, -1 se data1 < data2 e 0 se forem iguais.

*/
compare_dates(D1, D2, C) :- D1 @< D2 -> C is -1, !.
compare_dates(D1, D2, C) :- D1 @> D2 -> C is 1, !.
compare_dates(_, _, C) :- C is 0.

string_to_date(String, Date) :- split_string(String, '/', '', [D, M, A]),
    atom_number(D, Dia),
    atom_number(M, Mes),
    atom_number(A, Ano),
    Date = date(Ano, Mes, Dia, 0, 0, 0.0, 10800, -, -).