:- encoding(utf8).

%
:- use_module('./Utils/utils.pro').
:- use_module('./Utils/time.pro').
:- use_module('./Models/model.pro').
:- use_module('./Controllers/pacienteController.pro').
:- use_module('./Controllers/medicoController.pro').
:- use_module('./Controllers/ubsController.pro').

begin :- model:iniciaSistema,
         main.

/* Menu inicial. */
main :-
    writeln( '-----------------------------------------------------------------'),
    writeln( '         SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS)         '),
    writeln( '-----------------------------------------------------------------'),
    writeln('--------------'),
    writeln('  (L)ogin     '),
    writeln('  (C)adastrar '),
    writeln('  (E)ncerrar  '),
    writeln('--------------'),

    promptString('Opção > ', OP),
    ( OP = "L" -> tty_clear, login;
      OP = "C" -> tty_clear, cadastro;
      OP = "E" -> writeln('Tchau');
      writeln('Opção Inválida'), nl, promptString('Pressione qualquer tecla para continuar', _), tty_clear, main).

/* Menu de login. */
login :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - LOGIN  \n
-----------------------------------------------------------------\n'),
         prompt('Insira seu ID > ', ID),
         promptString('Insira sua senha > ', S), autentica(ID, S, T),
         (T =:= 0 -> tty_clear, menuPaciente(ID);
          T =:= 1 -> tty_clear, menuUBS(ID);
          T =:= 2 -> tty_clear, menuMedico(ID) ;
          write('ID não encontrado\n'), login).

/* Menu de cadastro. */
cadastro :- write('-----------------------------------------------------------------\n
   SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - CADASTRO  \n
-----------------------------------------------------------------\n'),
    write('(P)aciente'), nl, write('(U)BS'), nl, write('(V)oltar'), nl,
    promptString('Opção > ', O),
    (O = "P" -> cadastraPac, tty_clear, main;
     O = "U" -> cadastraUBS, tty_clear, main;
     O = "V" -> tty_clear, main;
     write('Opção inválida\n'), cadastro).

/* Lê as opções de cadastro do paciente e faz seu cadastro. */
cadastraPac :- promptString('Nome > ', Nome),
               promptString('CPF > ', CPF),
               promptString('Data de Nascimento > ', Nascimento),
               prompt('Peso > ', Peso),
               prompt('Altura > ', Altura),
               promptString('Tipo Sanguíneo > ', Sangue),
               promptString('Endereço > ', Endereco),
               promptString('Cardiopata (n/s) > ', Cardiopata),
               promptString('Diabético (n/s) > ', Diabetico),
               promptString('Hipertenso (n/s) > ', Hipertenso),
               promptString('Senha > ', Senha),
               model:nextId(N),
               assertz(model:paciente(N, Nome, CPF, Nascimento, Peso, Altura, Sangue, Endereco, Cardiopata, Diabetico, Hipertenso)),
               assertz(model:logins(N, Senha, 0)),
               format('\nCadastrado de paciente realizado com sucesso, id: ~d', [N]),
               promptString('\n\nPressione qualquer tecla para continuar', _).

/* Lê as opções de cadastro da UBS e faz seu cadastro. */
cadastraUBS :- promptString('Nome > ', Nome),
               promptString('Endereço > ', Endereco),
               promptString('Senha > ', Senha),
               model:nextId(N),
               assertz(model:ubs(N, Nome, Endereco)),
               assertz(model:logins(N, Senha, 1)),
               format('\nCadastrado de UBS realizado com sucesso, id: ~d', [N]),
               promptString('\n\nPress to continue', _). 

/* 
Menu do paciente. 

@param ID o id do paciente.
*/
menuPaciente(ID) :- write('---------------------------------------------------------------------------------\n
        SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU PACIENTE \n
---------------------------------------------------------------------------------\n'),
                write('(B)uscar unidade'), nl,
                write('(R)equisitar'), nl,
                write('(C)onsultar'), nl,
                write('(E)mergência'), nl,
                write('(S)air'), nl,
                promptString('Opção > ', O),
                (O = "B" -> menuPacienteBuscarUBS(ID), tty_clear, menuPaciente(ID);
                O = "R" -> menuPacienteRequisitar(ID), tty_clear, menuPaciente(ID);
                O = "C" -> menuPacienteConsultar(ID), tty_clear, menuPaciente(ID);
                O = "E" -> menuPacienteEmergencia, tty_clear, menuPaciente(ID);
                O = "S" -> tty_clear, main;
                write('Opcao Invalida\n'), menuPaciente(ID)).

menuPacienteBuscarUBS(ID) :- write('(T)odas as UBS'), nl,
                            write('(E)specilidades da UBS'), nl,
                            write('(U)BS por especialidade'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> menuPacienteBuscarTodasUbs, utils:mensagemEspera, tty_clear, menuPaciente(ID);
                            O = "E" -> menuPacienteBuscarEspecialidadesUbs, utils:mensagemEspera, tty_clear, menuPaciente(ID);
                            O = "U" -> menuPacienteBuscarUBSPorEspecialidade, utils:mensagemEspera, tty_clear, menuPaciente(ID);
                            write('Opcao Invalida\n'), menuPaciente(ID)).

menuPacienteBuscarUBSPorEspecialidade :- promptString('Especialidade > ', E), paciente:buscarUnidadesEspec(E, Id).

menuPacienteBuscarTodasUbs :- buscarTodasUnidades.

menuPacienteBuscarEspecialidadesUbs :- promptString('Id da UBS > ', I), paciente:especialidadeDaUBS(I).

menuPacienteRequisitar(ID) :- write('(C)onsulta'), nl,
                              write('(E)xame'), nl,
                              write('(M)edicamento'), nl,
                              promptString('Opção > ', O),
                              (O ="C" -> leituraRequisitaConsulta(ID), tty_clear, menuPaciente(ID);
                              O = "E" -> leituraRequisitaExame(ID), tty_clear, menuPaciente(ID);
                              O = "M" -> leituraRequisitaMedicamento(ID), tty_clear, menuPaciente(ID);
                              write('Opção Inválida\n'), menuPacienteRequisitar(ID)).

leituraRequisitaConsulta(ID) :- prompt('ID do Médico > ', IDM),
                                medico:validaIDMedico(IDM),
                                prompt('ID da UBS > ', IDU),
                                ubs:validaIDUBS(IDU),
                                promptString('Informe a data desejada: ', Data),
                                model:nextId(N),
                                paciente:requisitarConsulta(N, ID, IDM, IDU, Data),
                                format('Consulta ~d Marcada com o Médico ~d, na UBS ~d e no dia ~w ~n', [N, IDM, IDU, Data]).

leituraRequisitaExame(ID) :- prompt('ID do Médico > ', IDM),
                             medico:validaIDMedico(IDM),
                             prompt('ID da UBS > ', IDU),
                             ubs:validaIDUBS(IDU),
                             promptString('Tipo de Exame > ', Tipo),
                             promptString('Informe a data desejada: ', Data),
                             model:nextId(N),
                             paciente:requisitarExame(N, ID, IDM, IDU, Tipo, Data),
                             format('Exame ~d marcado na UBS ~d e no dia ~w ~n', [N, IDU, Data]).

leituraRequisitaMedicamento(ID) :- prompt('ID da Receita > ', IDR),
                                    ubs:validaIDReceita(IDR),
                                    paciente:requisitarMedicamento(IDR, R),
                                    writeln(R).

menuPacienteConsultar(ID) :- write('(L)audo'), nl,
                             write('(R)eceita'), nl,
                             write('(E)xame'), nl,
                             write('(C)onsultas'), nl,
                             promptString('Opção > ', O),
                             (O = "L" -> leituraConsultaLaudo(ID), tty_clear, menuPaciente(ID);
                             O = "R" -> leituraConsultaReceita, tty_clear, menuPaciente(ID);
                             P = "E" -> leituraConsultaExame, tty_clear, menuPaciente(ID);
                             O = "C" -> leituraConsultaConsultas, tty_clear, menuPaciente(ID);
                             write('Opção Inválida\n'), menuPacienteConsultar(ID)).

leituraConsultaLaudo(ID) :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosLaudos(ID);
                            O = "E" -> leituraConsultaLaudoEspecifico(ID);
                            write('Opção Inválida\n'), leituraConsultaLaudo(ID)).

leituraConsultaTodosLaudos(ID) :- forall(paciente:consultarLaudos(ID, IDL, IDM, IDU, Dia), 
                                  format('Laudo ~d: ~d, ~d, ~w', [IDL, IDM, IDU, Dia])).

leituraConsultaLaudoEspecifico(ID) :- prompt('ID do Laudo > ', IDL),
                                      ubs:validaIDLaudo(IDL),
                                      paciente:consultaLaudo(ID, IDL, IDM, IDU, Dia),
                                      format('Laudo ~d: ~d, ~d, ~w', [IDL, IDM, IDU, Dia]).

leituraConsultaReceita :- write('(T)odas'), nl,
                              write('(E)specífica'), nl,
                              promptString('Opção > ', O),
                              (O = "T" -> leituraConsultaTodasReceitas;
                              O = "E" -> leituraConsultaReceitaEspecifica;
                              write('Opção Inválida\n'), leituraConsultaReceita).

leituraConsultaTodasReceitas :- forall(paciente:consultarReceitas(IDR, IDM, IDU), 
                                    format('Receita ~d: ~d, ~d, ~w', [IDR, IDM, IDU])).

leituraConsultaReceitaEspecifica :- prompt('ID da Receita > ', IDR),
                                        ubs:validaIDReceita(IDR),
                                        paciente:consultarReceita(IDR, IDM, IDU),
                                        format('Receita ~d: ~d, ~d, ~w', [IDR, IDM, IDU]).

leituraConsultaExame :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosExames;
                            O = "E" -> leituraConsultaExameEspecifico;
                            write('Opção Inválida\n'), leituraConsultaExame).

leituraConsultaTodosExames :- forall(paciente:consultarExames(IDE, IDM, T, Dia, Resultado), 
                                  format('Exame ~d: ~d, ~w, ~w, ~w', [IDE, IDM, T, Dia, Resultado])).

leituraConsultaExameEspecifico :- prompt('ID do Exame > ', IDE),
                                      paciente:consultarExame(IDE, IDM, T, Dia, Resultado),
                                      format('Exame ~d: ~d, ~w, ~w, ~w', [IDE, IDM, T, Dia, Resultado]).

leituraConsultaConsultas :- write('(T)odas'), nl,
                                write('(E)specífica'), nl,
                                promptString('Opção > ', O),
                                (O = "T" -> leituraConsultaTodasConsultas;
                                O = "E" -> leituraConsultaConsultaEspecifica;
                                write('Opção Inválida\n'), leituraConsultaReceita).

leituraConsultaTodasConsultas :- forall(paciente:consultarConsultas(IDC, IDM, IDU, Dia), 
                                format('Consulta ~d: ~d, ~d, ~w', [IDC, IDM, IDU, Dia])).

leituraConsultaConsultaEspecifica :- prompt('ID da Consulta > ', IDC),
                                     paciente:consultarConsulta(IDC, IDM, IDU, Dia), 
                                     format('Consulta ~d: ~d, ~d, ~w', [IDC, IDM, IDU, Dia]).

menuPacienteEmergencia :- promptString('Endereço > ', E),
                          paciente:emergencia(E).

/* Menu da UBS. */
menuUBS(IdUBS) :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - UBS  \n
-----------------------------------------------------------------\n'),
    write('(C)adastrar corpo médico'), nl, write('(V)isualizar informações'), nl,
    write('(F)armácia'), nl, write('(D)ashboard'), nl, write('(S)air'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> cadastraMedico(IdUBS) ;
     Op = "V" -> visualizaUBS(IdUBS) ;
     Op = "F" -> farmaciaUBS(IdUBS) ;
     Op = "D" -> dashBoard(IdUBS) ;
     Op = "S" -> tty_clear, main;
     write('Opção inválida\n'), menuUBS(IdUBS)).

cadastraMedico(IdUBS) :- promptString('Nome > ', Nome),
    promptString('CRM > ', CRM),
    promptString('Especialidade > ', Especialidade),
    promptString('Senha > ', Senha),
    model:nextId(N),
    assertz(model:medico(N, Nome, CRM, IdUBS, Especialidade)),
    assertz(model:logins(N, Senha, 2)),
    format('----------------------------~nMEDICO ~d~nNome: ~w~nCRM: ~w~nUBS: ~d~nEspecialidade: ~w~n', [N, Nome, CRM, IdUBS, Especialidade]),
    utils:mensagemEspera, tty_clear, menuUBS(IdUBS). 

visualizaUBS(IdUBS) :- write('(A)gendamentos'), nl, write('(P)aciente'), nl, write('(M)édico'), nl,
    promptString('Opção > ', Op),
    (Op = "A" -> forall(ubs:visualizaConsultasFuturas(IdUBS, IdConsulta, IdPaciente, IdMedico, Dia), 
    format('----------------------------~nCONSULTA ~d~nPaciente: ~d~nMédico responsável: ~d~nUBS: ~d~nData: ~w~n', [IdConsulta, IdPaciente, IdMedico, IdUBS, Dia])),
    utils:mensagemEspera, tty_clear, menuUBS(IdUBS);
    Op = "P" -> forall(ubs:visualizaPacientes(IdUBS, IdPaciente, Nome, Endereco, CPF, Dia, Peso, Altura, TipoSanguineo, C, D, H),
    format('----------------------------~nPACIENTE ~d~nNome: ~w~nEndereço: ~w~nCPF: ~w~nData de Nascimento: ~w~nPeso: ~d - Altura: ~d~wTipo sanguíneo: ~w~d~w/~w/~w', [IdPaciente, Nome, Endereco, CPF, Dia, Peso, Altura, TipoSanguineo, C, D, H])),
    utils:mensagemEspera, tty_clear, menuUBS(IdUBS);
    Op = "M" -> visualizaUBSMedicos(IdUBS), menuUBS(IdUBS);
    write('Opção inválida\n'), menuUBS(IdUBS)).

visualizaUBSMedicos(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> forall(ubs:visualizaMedicos(IdUBS, IdMed, Nome, CRM, Especialidade),
    format('----------------------------~nMEDICO ~d~nNome: ~w~nCRM: ~w~nUBS: ~d~nEspecialidade: ~w~n', [IdMed, Nome, CRM, IdUBS, Especialidade])),
    utils:mensagemEspera, tty_clear;
    Op = "E" -> (visualizaUBSMedico(IdUBS);
    write('Opção inválida\n'))).

visualizaUBSMedico(IdUBS) :- prompt('Id Médico > ', IdMed), 
    (medico:validaIDMedico(IdMed) -> ubs:visualizaMedico(IdUBS, IdMed, Nome, CRM, Especialidade), 
    format('----------------------------~nMEDICO ~d~nNome: ~w~nCRM: ~w~nUBS: ~d~nEspecialidade: ~w~n', [IdMed, Nome, CRM, IdUBS, Especialidade]), utils:mensagemEspera;
    format('ID: ~d inválido\n', [IdMed])).

farmaciaUBS(IdUBS) :- write('(C)onsultar'), nl, write('(N)ovo'), nl, write('(A)dicionar'), nl, write('(R)emover'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> farmaciaUBSConsultar(IdUBS), tty_clear, menuUBS(IdUBS);
    Op = "N" -> novoMedicamento(IdUBS), tty_clear, menuUBS(IdUBS);
    Op = "A" -> adicionaMedicamentoEstoque(IdUBS), tty_clear, menuUBS(IdUBS);
    Op = "R" -> removeMedicamentoEstoque(IdUBS), tty_clear, menuUBS(IdUBS);
    write('Opção inválida\n'), menuUBS(IdUBS)).

farmaciaUBSConsultar(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> forall(ubs:consultarMedicamentos(IdMed, IdUBS, Nome, Qtd, Bula),
    format('----------------------------~nMEDICAMENTO ~d~nNome: ~w~nUBS: ~d~nQuantidade: ~d~nBula: ~w~n', [IdMed, IdUBS, Nome, Qtd, Bula])),
    utils:mensagemEspera;
    Op = "E" -> (promptString('Id Medicamento > ', IdMed), ubs:validaIDMedicamento(IdMed),
    ubs:consultarMedicamento(IdMed, IdUBS, Nome, Qtd, Bula),
    format('----------------------------~nMEDICAMENTO ~d~nNome: ~w~nUBS: ~d~nQuantidade: ~d~nBula: ~w~n', [IdMed, IdUBS, Nome, Qtd, Bula]),
    utils:mensagemEspera;
    write('Opção inválida\n'))).

novoMedicamento(IdUBS) :- promptString('Nome > ', Nome),
    promptString('Nome > ', Nome),
    prompt('Quantidade > ', Qtd),
    promptString('Bula > ', Bula),
    model:nextId(N),
    assertz(model:medicamento(N, IdUBS, Nome, Qtd, Bula)),
    format('----------------------------~nMEDICAMENTO ~d~nNome: ~w~nUBS: ~d~nQuantidade: ~d~nBula: ~w~n', [N, IdUBS, Nome, Qtd, Bula]),
    utils:mensagemEspera. 

adicionaMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a adicionar > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:adicionaMedicamentoEstoque(IdMed, IdUBS, Nome, Qtd, Bula),
    format('----------------------------~nMEDICAMENTO ~d~nNome: ~w~nUBS: ~d~nQuantidade: ~d~nBula: ~w~n', [IdMed, IdUBS, Nome, Qtd, Bula]),
    utils:mensagemEspera;
    format('ID: ~d inválido\n', [IdMed])).

removeMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a remover > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:adicionaMedicamentoEstoque(IdMed, IdUBS, Nome, Qtd, Bula),
    format('----------------------------~nMEDICAMENTO ~d~nNome: ~w~nUBS: ~d~nQuantidade: ~d~nBula: ~w~n', [IdMed, IdUBS, Nome, Qtd, Bula]),
    utils:mensagemEspera;
    format('ID: ~d inválido\n', [IdMed])).

dashBoard(_).

/* Menu do medico. */
menuMedico(IDM) :- write('---------------------------------------------------------------------------------\n
        SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - MENU MEDICO \n
---------------------------------------------------------------------------------\n'),
                write('(I)nforma Horários'), nl,
                write('(A)cessar Dados'), nl,
                write('(E)mitir'), nl,
                write('(T)ransferência'), nl,
                write('(S)air'), nl,
                promptString('Opção > ', O),
                (O = "I" -> time:medicoHorarios(IDM), mensagemEspera, menuMedico(IDM);
                O = "A" -> menuMedicoAcessarDados(IDM), mensagemEspera, menuMedico(IDM);
                O = "E" -> menuMedicoEmitir(IDM), mensagemEspera, menuMedico(IDM);
                O = "T" -> menuMedicoTransferencia(IDM), mensagemEspera, menuMedico(IDM);
                O = "S" -> tty_clear, main;
                write('Opcao Invalida\n'), mensagemEspera, menuMedico(IDM)).

menuMedicoAcessarDados(IDM) :- write('(P)acientes'), nl,
                               write('(E)xames'), nl,
                               write('(C)onsultas agendadas'), nl,
                               write('(M)edicamentos'), nl,
                               write('(V)oltar'), nl,
                               promptString('Opção > ', O),
                               (O = "P" -> acessarDadosPacientes, mensagemEspera, menuMedico(IDM);
                               O = "E" -> menuMedicoAcessarExames(IDM), mensagemEspera, menuMedico(IDM);
                               O = "A" -> menuMedicoAcessarAgendamentos(IDM), mensagemEspera, menuMedico(IDM);
                               O = "M" -> menuMedicoAcessarMedicamentos(IDM), mensagemEspera, menuMedico(IDM);
                               O = "V" -> tty_clear, menuMedico(IDM) ;
                               write('Opção Inválida\n\n'), menuMedicoAcessarDados(IDM)).

acessarDadosPacientes :- prompt('ID do Paciente > ', ID),
                         (paciente:validaIDPaciente(ID), medico:acessarDadosPaciente(ID) ; write('ID informado é inválido!\n')).
 
menuMedicoAcessarExames(IDM) :- write('(T)odos'), nl,
                                write('(E)specíficos'), nl,
                                write('(V)oltar'), nl,
                                promptString('Opção > ', O),
                                (O = "T" -> medico:acessarExames(IDM, 0);
                                O = "E" -> menuMedicoAcessarExamesEspecifico;
                                O = "V" -> tty_clear, menuMedicoAcessarDados(IDM);
                                write('Opção Inválida\n\n')).

menuMedicoAcessarExamesEspecifico :- write('ID do (E)xame'), nl,
                                    write('ID do (P)aciente'), nl,
                                    promptString('Opção > ', O),
                                    prompt('Insira o ID > ', ID),
                                    ((O = "E", ubs:validaIDExame(ID)) -> medico:acessarExames(ID, 1);
                                    (O = "P", paciente:validaIDPaciente(ID)) -> medico:acessarExames(ID, 2);
                                    write('Opção ou ID informado é inválido!\n\n')).


menuMedicoAcessarAgendamentos(IDM) :- write('(T)odos'), nl,
                                      write('(E)specíficos'), nl,
                                      write('(V)oltar'), nl,
                                      promptString('Opção > ', O),
                                      (O = "T" -> medico:acessarConsultas(IDM, 0);
                                      O = "E" -> menuMedicoAcessarAgendamentosEspecifico(IDM);
                                      O = "V" -> tty_clear, menuMedicoAcessarDados(IDM);
                                      write('Opção Inválida\n\n')).                  

menuMedicoAcessarAgendamentosEspecifico(IDM) :- write('Data > '), nl,
                                                promptString('Data > ', Data),
                                                (medico:acessarConsultas(IDM, Data); write('Data Inválida\n')).

menuMedicoAcessarMedicamentos(IDM) :- write('(T)odos'), nl,
                                      write('(E)specíficos'), nl,
                                      write('(V)oltar'), nl,
                                      promptString('Opção > ', O),
                                      (O = "T" -> medico:acessarMedicamentos ;
                                      O = "E" -> prompt('Insira o ID > ', ID), medico:acessarMedicamentos(ID) ;
                                      O = "V" -> tty_clear, menuMedicoAcessarDados(IDM);
                                      write('Opção Inválida\n\n')).

menuMedicoEmitir(IDM) :- write('(R)eceita'), nl,
                         write('Resultado de (E)xame'), nl,
                         write('(L)audo Médico'), nl,
                         write('(V)oltar'), nl,
                         promptString('Opção > ', O),
                         (O = "R" -> menuMedicoEmitirReceita(IDM);
                         O = "E" -> menuMedicoEmitirResultado;
                         O = "L" -> menuMedicoEmitirLaudo(IDM);
                         O = "V" -> tty_clear, menuMedico(IDM) ;
                         write('Opção Inválida\n'), menuMedicoEmitir(IDM)).
                                                      
menuMedicoEmitirReceita(IDM) :- ((prompt('ID do Paciente > ', ID), paciente:validaIDPaciente(ID)) ->  medico:emitirReceita(IDM, ID)) ; write('ID informado é inválido!\n\n').

menuMedicoEmitirResultado :- ((prompt('ID do Exame > ', IdExame), ubs:validaIDExame(IdExame)) -> medico:emitirResultadoExame(IdExame)) ; write('ID informado é inválido!\n\n').

menuMedicoEmitirLaudo(IDM) :- ((prompt('ID do Exame > ', IdExame), ubs:validaIDExame(IdExame)) -> medico:emitirLaudo(IDM, IdExame)) ; write('ID informado é inválido!\n\n').   

menuMedicoTransferencia(IDM) :- prompt('ID da UBS de destino > ', IdUBS),
                                ubs:validaIDUBS(IdUBS), medico:solicitarTransferencia(IDM, IdUBS) ; write('ID informado é inválido!\n').
