:- module(model, [iniciaPaciente/0, iniciaUBS/0, iniciaMedico/0,
                  iniciaReceita/0, iniciaMedicamento/0, iniciaLaudo/0,
                  iniciaExame/0, iniciaId/0, iniciaLogins/0, iniciaSistema/0, nextId/1]).

/*

Inicializa a tabela de pacientes.
Os campos são: id :: Int, nome :: String, cpf :: String,
dataNascimento :: String, peso :: Double, altura :: Double,
tipoSanguineo :: String, endereco :: String, cardiopata :: Bool,
diabetico :: Bool, hipertenso :: Bool.

*/
iniciaPaciente :- dynamic paciente/11.

/*

Inicializa a tabela de UBS.
Os campos são: id :: Int, nome :: String, endereco :: String.

*/
iniciaUBS :- dynamic ubs/3.

/*

Inicializa a tabela de médico e a tabela auxiliar dos horários.
Os campos de médico são: id :: Int, nome :: String, crm :: String,
idUbs :: Int, especialidade :: String.

Os campos de horário são: idMed :: Int, horario :: TBD.

*/
iniciaMedico :- dynamic medico/5, medico_horarios/2.

/*

Inicializa a tabela de receita e a tabela auxiliar dos remédios de uma receita.
Os campos de receita são: id :: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int.

Os campos de remédio são: idReceita :: Int, idMedicamento :: Int, instruções :: String,
quantidade :: Int.

*/
iniciaReceita :- dynamic receita/4, receita_remedio/4.

/*

Inicializa a tabela de medicamentos.
Os campos são: id :: Int, idUBS :: Int, nome :: String, qtdEstoque :: Int, bula :: String.

*/
iniciaMedicamento :- dynamic medicamento/5.

/*

Inicializa a tabela de laudos.
Os campos são: id :: Int, idMed :: Int, idExame :: Int, texto :: String.

*/
iniciaLaudo :- dynamic laudo/4.

/*

Inicializa a tabela de exame.
Os campos são: id :: Int, idPaciente :: Int, idMedico :: Int, idUBS :: Int,
tipo :: String, dia :: TBD, resultado :: String.

*/
iniciaExame :- dynamic exame/7.

/*

Inicializa a tabela de logins.
Os campos são: id :: Int, senha :: String, tipoUsuário :: Int.

Tipo de usuário: 0 - Paciente, 1 - UBS, 2 - Médico.

*/
iniciaLogins :- dynamic logins/3.

/*

Inicializa os ids do sistema, contando do 0.

*/
iniciaId :- asserta(id(0)).

/*

Pega o próximo ID do sistema.
nextId(+N).

*/
nextId(N) :- id(X), retract(id(X)), N is X + 1, asserta(id(N)).


/*

Inicializa todas as tabelas do sistema de uma só vez.

*/
iniciaSistema :- iniciaPaciente, iniciaUBS, iniciaMedico, iniciaReceita,
                 iniciaMedicamento, iniciaLaudo, iniciaExame, iniciaLogins,
                 iniciaId.

