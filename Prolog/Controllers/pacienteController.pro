:- module(paciente, [buscarUnidadesEspec/1, buscarTodasUnidades/0, especialidadeDaUBS/1]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/utils.pro').

/*
Buscar as unidades que tem determinada especialidade.

@param E: especialidade buscada.


*/
buscarUnidadesEspec(E) :- model:medico(_,_,_,IdUbs,E), forall(model:ubs(IdUbs, Nome, End), show:showUbs(model:ubs(IdUbs, Nome, End))).

/* Listar todas as unidades. */
buscarTodasUnidades :- forall(model:ubs(IdUbs, Nome, End), show:showUbs(model:ubs(IdUbs, Nome, End)).

/* Listar todas as especialidades de uma UBS. */
especialidadeDaUBS(I) :- model:ubs(I, _, _), format('Especialidades UBS ~d~n', [I]), forall(model:medico(Id, Nome, CRM, I, Especialidade), format('~w~n', [Especialidade]))).

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
*/
requisitarMedicamento(ID) :- model:receita_remedio(ID, IdMed, _,Qtd), model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula),
    NovaQuantidade is Estoque - Qtd, 
    retract(model:medicamento(IdMed, IdUBS, Nome, Estoque, Bula)),
    assertz(model:medicamento(IdMed, IdUBS, Nome, NovaQuantidade, Bula)).
    

/* 
Listar todos os laudos do paciente. 

@param IDPac: id do paciente. 

*/
consultarLaudos(IDPac) :- model:exame(IdEx, IDPac,_,_,_,_,_), forall(model:laudo(Id, IdMed, IdEx, Text), show:showLaudo(model:laudo(Id, IdMed, IdEx, Text))).

/* 
Ver um laudo específico do paciente. 

@param IDPac: id do paciente.
@param IDLaudo: id do laudo.

*/
consultarLaudo(IDPac, IDLaudo) :- model:laudo(IdLaudo, IdMed, IdEx, Text), show:showLaudo(model:laudo(IdLaudo, IdMed, IdEx, Text)).

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
consultarReceita(IdPac, IdReceita) :- model:receita(IdReceita, IDPac, IdMed, IdUbs), show:showReceita(model:receita(IdReceita, IDPac, IdMed, IdUbs)).

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
consultarConsulta(IDPAC, IDC) :- model:consulta(IDC, IDPac, IdMed, IdUBS, Data), show:showConsulta(model:consulta(IDC, IDPac, IdMed, IdUBS, Data)).

/* 
Recebe um pedido de emergência. 

@param E: endereço da emergência.
*/
emergencia(E) :- ansi_format([bold, fg(green)], 'Uma ambulância está a caminho!', []), nl,
              ansi_format([bold, fg(green)], 'wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo', []).

/* 
Valida se um id pertence a um paciente. 

@param ID: id do paciente.
*/
validaIDPaciente(ID).