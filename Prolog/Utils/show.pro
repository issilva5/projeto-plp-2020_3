:- module(show, [showPaciente/1, showExame/1, showMedicamento/1, showConsulta/1, showMedico/1]).

:- use_module('../Models/model.pro').

showPaciente(model:paciente(IdPac, Nome, _, Nascimento, Peso, Altura, Sangue, Endereco, Card, Diab, Hiper)) :-
    write('----------------------------'), nl,
    format('PACIENTE ~d~n', [IdPac]),
    format('Nome: ~w~n', [Nome]),
    format('Endereço: ~w~n', [Endereco]),
    format('Data de nascimento: ~w~n', [Nascimento]),
    format('Peso/Altura: ~d/~d~n', [Peso, Altura]),
    format('Tipo sanguíneo: ~w~n', [Sangue]),
    format('C/D/H: ~w/~w/~w', [Card, Diab, Hiper]).

showExame(model:exame(IdEx, IdPac, IDM, IdUBS, Tipo, Data, Resultado)) :-
    write('----------------------------'), nl,
    format('EXAME ~d', [IdEx]),
    format('Paciente: ~d', [IdPac]),
    format('Médico responsável: ~d', [IDM]),
    format('UBS: ~d', [IdUBS]),
    format('Tipo do exame: ~w', [Tipo]),
    write('Data: '), format_time(user, '%a, %d %b %Y %T', Data), nl,
    format('Resultado: ', [Resultado]).

showConsulta(model:consulta(ID, IdPac, IDM, IdUBS, Data)) :-
    write('----------------------------'), nl,
    format('CONSULTA ~d', [ID]),
    format('Paciente: ~d', [IdPac]),
    format('Médico responsável: ~d', [IDM]),
    format('UBS: ~d', [IdUBS]),
    write('Data: '), format_time(user, '%a, %d %b %Y %T', Data), nl.

showMedicamento(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula)) :-
    write('----------------------------'), nl,
    format('MEDICAMENTO ~d~n', [IdMed]),
    format('UBS: ~d~n', [IdUBS]),
    format('Nome: ~w~n', [Nome]),
    format('Bula: ~w~n', [Bula]),
    format('Quantidade em estoque: ~d~n', [Estoque]).

showMedico(model:medico(IdMed, Nome, CRM, IdUBS, Especialidade)) :-
    writeln('----------------------------'),
    format('MEDICO ~d~n', [IdMed]),
    format('Nome: ~w~n', [Nome]),
    format('CRM: ~w~n', [CRM]),
    format('UBS: ~d~n', [IdUBS]),
    format('Especialidade: ~w~n', [Especialidade]).