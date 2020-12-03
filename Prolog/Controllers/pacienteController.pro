:- module(paciente, [buscarUnidadesEspec/1, buscarTodasUnidades/0, especialidadeDaUBS/1, 
                    requisitarConsulta/5, requisitarExame/6, requisitarMedicamento/2,
                    consultarLaudos/1, consultarLaudo/1, consultarReceitas/1, consultarReceita/2,
                    consultarExames/1, consultarExame/2, consultarConsultas/1, consultarConsulta/2,
                    emergencia/1, validaIDPaciente/1]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/utils.pro').
:- use_module('../Controllers/ubsController.pro', [removeMedicamentoEstoque/3]).
:- use_module('../Persistence/persistence.pro', [escreveReceita/0, escreveReceitaRem/0]).

/*
Buscar as unidades que tem determinada especialidade.

@param E: especialidade buscada.


*/
buscarUnidadesEspec(E) :- forall((model:medico(_,_,_,IdUbs,E), model:ubs(IdUbs, Nome, End)), show:showUbs(model:ubs(IdUbs, Nome, End))).

/* Listar todas as unidades. */
buscarTodasUnidades :- forall(model:ubs(IdUbs, Nome, End), show:showUbs(model:ubs(IdUbs, Nome, End))).

/* Listar todas as especialidades de uma UBS. */
especialidadeDaUBS(I) :- model:ubs(I, _, _), format('Especialidades UBS ~d~n', [I]), forall(model:medico(_, _, _, I, Especialidade), format('~w~n', [Especialidade])).

/* 
Cria uma consulta.

@param ID: id da consulta.
@param IDPac: id do paciente.
@param IDMed: id do médico.
@param IDUbs: id da UBS.
@param Dia: data da consulta.
*/
requisitarConsulta(ID, IDPac, IDMed, IDUbs, Dia) :- assertz(model:consulta(ID, IDPac, IDMed, IDUbs, Dia)).

/* 
Cria um exame. 

@param ID: id do exame.
@param IDPac: id do paciente.
@param IDMed: id do médico.
@param IDUbs: id da ubs.
@param Tipo: tipo do exame,.
@param Dia: dia do exame,.
*/
requisitarExame(ID, IDPac, IDMed, IDUbs, Tipo, Dia) :- assertz(model:exame(ID, IDPac, IDMed, IDUbs, Tipo, Dia, -)).

/* 
Deduz do estoque os medicamentos de um dada receita. 

@param ID: id da receita.

TODO reformular método.

*/
requisitarMedicamento(ID, IDPac) :- 
    forall((model:receita(ID, IDPac, IDMed, IdUbs),
    model:receita_remedio(ID, IdMedic, _, Qtd),
    model:medicamento(IdMedic, IdUbs, _, Estoque, _),
    Estoque >= Qtd
    ), ubs:removeMedicamentoEstoque(IdMedic, IdUbs, Qtd)),
    retract(model:receita(ID, IDPac, _, _)),
    retractall(model:receita_remedio(ID, _, _, _)),
    persistence:escreveReceita,
    persistence:escreveReceitaRem.

/* 
Listar todos os laudos do paciente. 

@param IDPac: id do paciente. 

*/
consultarLaudos(IDPac) :- forall((model:exame(IdEx, IDPac,_,_,_,_,_), model:laudo(Id, IdMed, IdEx, Text)), show:showLaudo(model:laudo(Id, IdMed, IdEx, Text))).

/* 
Ver um laudo específico do paciente. 

@param IDPac: id do paciente.
@param IDLaudo: id do laudo.

*/
consultarLaudo(IDLaudo) :- model:laudo(IDLaudo, IdMed, IdEx, Text), show:showLaudo(model:laudo(IDLaudo, IdMed, IdEx, Text)).

/* 
Listar todas as receitas do paciente. 

@param IDPac: id do paciente.

*/
consultarReceitas(IDPac) :- forall(model:receita(Id, IDPac, IdMed, IdUbs), show:showReceita(model:receita(Id, IDPac, IdMed, IdUbs))).

/* 
Ver uma receita específica do paciente. 

@param IDPac: id do paciente.
@param IdReceita: id da receita.

*/
consultarReceita(IDPac, IdReceita) :- model:receita(IdReceita, IDPac, IdMed, IdUbs), show:showReceita(model:receita(IdReceita, IDPac, IdMed, IdUbs)).

/* 
Listar todos os exames do paciente. 

@param IDPac: id do paciente.

*/
consultarExames(IDPac) :- forall(model:exame(IdEx, IDPac, IDM, IdUBS, Tipo, Data, Resultado), show:showExame(model:exame(IdEx, IDPac, IDM, IdUBS, Tipo, Data, Resultado))).

/* 
Ver um exame específico do paciente. 

@param IDPac: id do paciente.
@param IdEx: id do Exame.

*/
consultarExame(IDPac, IdEx) :- model:exame(IdEx, IDPac, IDM, IdUBS, Tipo, Data, Resultado), show:showExame(model:exame(IdEx, IDPac, IDM, IdUBS, Tipo, Data, Resultado)).

/* 
Listar todas as consultas do paciente. 

@param IDPac: id do paciente. id:: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int, dia :: String

*/
consultarConsultas(IDPac) :- forall(model:consulta(IDC, IDPac, IdMed, IdUBS, Data), show:showConsulta(model:consulta(IDC, IDPac, IdMed, IdUBS, Data))).

/* 
Ver uma consulta específica do paciente. 

@param IDPac: id do paciente.
@param IDC: id da consulta.

*/
consultarConsulta(IDPac, IDC) :- model:consulta(IDC, IDPac, IdMed, IdUBS, Data), show:showConsulta(model:consulta(IDC, IDPac, IdMed, IdUBS, Data)).

/* 
Recebe um pedido de emergência. 

@param E: endereço da emergência.
*/
emergencia(E) :- ansi_format([bold, fg(green)], 'Uma ambulância está a caminho do endereço ~w!', [E]), nl,
              ansi_format([bold, fg(green)], 'wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo', []).

/* 
Valida se um id pertence a um paciente. 

@param ID: id do paciente.
*/
validaIDPaciente(ID) :- model:paciente(ID, _, _, _, _, _, _, _, _, _, _).