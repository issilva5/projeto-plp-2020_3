:- module(paciente, [buscarTodasUnidades/0, especialidadeDaUBS/1, buscarUnidadesEspec/2, requisitarConsulta/5, requisitarExame/6, 
                    requisitarMedicamento/2, consultarLaudos/5, consultarLaudo/5, consultarReceitas/3,
                    consultarReceita/3, consultarExames/5, consultarExame/5, consultarConsultas/4,
                    consultarConsulta/4, emergencia/1, validaIDPaciente/1]).

:- use_module('../Models/model.pro').
:- use_module('../Utils/show.pro').
:- use_module('../Utils/utils.pro').

/*
Buscar as unidades que tem determinada especialidade.
@param E: especialidade buscada.
@param I: variável de resposta, Id da unidade.
@param U: nome da ubs do médico.
@param End: endereço da ubs.

Obs.: implementei esse método como um teste, pois ele ficará dando warning de que medico
não foi definido. Porém ao utilizar o método após definir ele funciona, então ignorem os
warnings.
*/
buscarUnidadesEspec(E, I) :- model:medico(_,_,_,I,E), forall(model:ubs(I, U, End), show:showUbs(model:ubs(I, U, End))).

/* Listar todas as unidades. */
buscarTodasUnidades :- forall(model:ubs(I, U, End), show:showUbs(model:ubs(I, U, End)).

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
requisitarExame(ID, IDPac, IDMed, IDUbs, Tipo, Dia) :- assertz(model:exame(ID, IDPac, IDMed, IDUbs, Tipo, Dia, '-')).

/* 
Deduz do estoque os medicamentos de um dada receita. 

@param ID: id da receita.
*/
requisitarMedicamento(ID, R).

/* 
Listar todos os laudos do paciente. 

@param IDPac: id do paciente.
@param IDL: id do laudo.
@param IDM: id do médico.
@param IDU: id da ubs.
@param Dia: dia.
*/
consultarLaudos(IDPac, IDL, IDM, IDU, Dia).

/* 
Ver um laudo específico do paciente. 

@param IDPac: id do paciente.
@param IDL: id do laudo.
@param IDM: id do médico.
@param IDU: id do ubs.
@param Dia: dia que o laudo foi gerado.
*/
consultarLaudo(IDPac, IDL, IDM, IDU, Dia).

/* 
Listar todas as receitas do paciente. 

@param IDR: id da receita.
@param IDM: id do médico.
@param IDU: id da ubs.
*/
consultarReceitas(IDR, IDM, IDU).

/* 
Ver uma receita específica do paciente. 

@param IDR: id da receita.
@param IDM: id do médico.
@param IDU: id da ubs.
*/
consultarReceita(IDR, IDM, IDU).

/* 
Listar todos os exames do paciente. 

@param IDE: id do exame.
@param IDM: id do médico.
@param T: tipo do exame.
@param Resultado: resultado do exame.
*/
consultarExames(IDE, IDM, T, Dia, Resultado).

/* 
Ver um exame específico do paciente. 

@param IDE: id do exame.
@param IDM: id do médico.
@param T: tipo do exame.
@param Resultado: resultado do exame.
*/
consultarExame(IDE, IDM, T, Dia, Resultado).

/* 
Listar todas as consultas do paciente. 

@param IDC: id da consulta.
@param IDM: id do médico.
@param IDU: id da ubs.
@param Dia: dia da consulta.
*/
consultarConsultas(IDC, IDM, IDU, Dia).

/* 
Ver uma consulta específica do paciente. 

@param IDC: id da consulta.
@param IDM: id do médico.
@param IDU: id da ubs.
@param Dia: dia da consulta.
*/
consultarConsulta(IDC, IDM, IDU, Dia).

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