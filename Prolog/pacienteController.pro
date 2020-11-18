:- module(paciente, [buscarUnidadesEspec/2]).

/*
Buscar as unidades que tem determinada especialidade.
@param I: variável de resposta, Id da unidade.
@param E: especialidade buscada.

Obs.: implementei esse método como um teste, pois ele ficará dando warning de que medico
não foi definido. Porém ao utilizar o método após definir ele funciona, então ignorem os
warnings.
*/
buscarUnidadesEspec(I, E) :- medico(_,_,_,I,E).

/* Listar todas as unidades. */
buscarTodasUnidades.

/* Listar todas as especialidades de uma UBS. */
especialidadeDaUBS.

/* Cria uma consulta. */
requisitarConsulta.

/* Cria um exame. */
requisitarExame.

/* Deduz do estoque os medicamentos de um dada receita. */
requisitarMedicamento.

/* Listar todos os laudos do paciente. */
consultarLaudos.

/* Ver um laudo específico do paciente. */
consultarLaudo.

/* Listar todas as receitas do paciente. */
consultarReceitas.

/* Ver uma receita específica do paciente. */
consultarReceita.

/* Listar todos os exames do paciente. */
consultarExames.

/* Ver um exame específico do paciente. */
consultarExame.

/* Listar todas as consultas do paciente. */
consultarConsultar.

/* Ver uma consulta específica do paciente. */
consultarConsulta.

/* Recebe um pedido de emergência. */
emergencia.

/* Valida se um id pertence a um paciente. */
validaIDPaciente.