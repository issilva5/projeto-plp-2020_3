:- encoding(utf8).

:- module(view, [main/0, begin/0, cadastro/0, login/0, cadastro/0, menuPaciente/1, menuUBS/1, menuMedico/1]).
%
:- use_module('./Utils/utils.pro').
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
                write('(B)uscar unidade por especialidade'), nl,
                write('(R)equisitar'), nl,
                write('(C)onsultar'), nl,
                write('(E)mergência'), nl,
                write('(S)air'), nl,
                promptString('Opção > ', O),
                (O = "B" -> menuPacienteBuscarUBSPorEspecialidade, tty_clear, menuPaciente(ID);
                O = "R" -> menuPacienteRequisitar(ID), tty_clear, menuPaciente(ID);
                O = "C" -> menuPacienteConsultar(ID), tty_clear, menuPaciente(ID);
                O = "E" -> menuPacienteEmergencia, tty_clear, menuPaciente(ID);
                O = "S" -> tty_clear, main;
                write('Opcao Invalida\n'), menuPaciente(ID)).

menuPacienteBuscarUBSPorEspecialidade :- promptString('Especialidade > ', E),
                                        forall(paciente:buscarUnidadesEspec(E, I, U, End), format('----------------------------~nUBS ~d~nNome: ~w~nEndereço: ~w~n', [I, U, End])).

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
    format('\nCadastrado de médico realizado com sucesso, id: ~d', [N]),
    promptString('\n\nPress to continue', _). 

visualizaUBS(IdUBS) :- write('(A)gendamentos'), nl, write('(P)aciente'), nl, write('(M)édico'), nl,
    promptString('Opção > ', Op),
    (Op = "A" -> ubs:visualizaConsultasFuturas(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "P" -> ubs:visualizaPacientes(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "M" -> visualizaUBSMedicos(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    write('Opção inválida\n'), tty_clear, menuUBS(IdUBS)).

visualizaUBSMedicos(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:visualizaMedicos(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "E" -> visualizaUBSMedico(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    write('Opção inválida\n'), tty_clear, menuUBS(IdUBS)).

visualizaUBSMedico(IdUBS) :- prompt('Id Médico > ', IdMed), 
    (medico:validaIDMedico(IdMed) -> ubs:visualizaMedico(IdUBS, IdMed); % Fazer a visualização
    format('ID: ~d inválido\n', [IdMed])).

farmaciaUBS(IdUBS) :- write('(C)onsultar'), nl, write('(N)ovo'), nl, write('(A)dicionar'), nl, write('(R)emover'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> farmaciaUBSConsultar(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "N" -> novoMedicamento(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "A" -> adicionaMedicamentoEstoque(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    Op = "R" -> removeMedicamentoEstoque(IdUBS), tty_clear, menuUBS(IdUBS); % Fazer a visualização
    write('Opção inválida\n'), menuUBS(IdUBS)).

farmaciaUBSConsultar(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:consultarMedicamentos(IdUBS); % Fazer a visualização
    Op = "E" -> promptString('Id Medicamento > ', IdMed), ubs:validaIDMedicamento(IdMed),ubs:consultarMedicamento(IdUBS, IdMed);
    write('Opção inválida\n')).

novoMedicamento(IdUBS) :- promptString('Nome > ', Nome),
    promptString('Nome > ', Nome),
    prompt('Quantidade > ', Quantidade),
    promptString('Bula > ', Bula),
    model:nextId(N),
    assertz(model:medicamento(N, IdUBS, Nome, Quantidade, Bula)),
    format('\nCadastrado de medicamento realizado com sucesso, id: ~d', [N]),
    promptString('\n\nPress to continue', _). 

adicionaMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a adicionar > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:adicionaMedicamentoEstoque(IdUBS, IdMed, Qtd);
    format('ID: ~d inválido\n', [IdMed])).

removeMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a remover > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:removeMedicamentoEstoque(IdUBS, IdMed, Qtd);
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
                (O = "I" -> menuMedicoInformarHorario(IDM), tty_clear, menuMedico(ID);
                O = "A" -> menuMedicoAcessarDados(IDM), tty_clear, menuMedico(ID);
                O = "E" -> menuMedicoEmitir(IDM), tty_clear, menuMedico(ID);
                O = "T" -> menuMedicoTransferencia(IDM), tty_clear, menuMedico(ID);
                O = "S" -> tty_clear, login;
                write('Opcao Invalida\n'), menuMedico(IDM)).

/*menuMedicoInformarHorario(IDM)*/

menuMedicoAcessarDados(IDM) :- write('(P)acientes'), nl,
                               write('(E)xames'), nl,
                               write('(A)gendamento'), nl,
                               write('(M)edicamentos'), nl,
                               promptString('Opção > ', O), % Fazer a visualização
                               (O = "P" -> acessarDadosPacientes(ID), tty_clear, menuMedico(IDM);
                               O = "E" -> menuMedicoAcessarExames(IDM), tty_clear, menuMedico(IDM);
                               P = "A" -> menuMedicoAcessarAgendamentos(IDM), tty_clear, menuMedico(IDM);
                               O = "M" -> consultarMedicamentos(IDM), tty_clear, menuMedico(IDM);
                               write('Opção Inválida\n'), menuMedicoAcessarDados(IDM)).

acessarDadosPacientes(ID) :- prompt('ID do Paciente > ', ID),
                             paciente:validaIDPaciente(ID),
                             /*write('ID informado é inválido!\n'), acessarDadosPaciente(ID)).*/
                             medico:acessarDadosPaciente(ID).
 
menuMedicoAcessarExames(IDM) :- write('(T)odos'), nl,
                                write('(E)specíficos'), nl,
                                promptString('Opção > ', O),
                                (O = "T" -> medico:acessarExames(IdExame), tty_clear, menuMedico(IDM);
                                O = "E" -> menuMedicoAcessarExamesEspecifico(IdExame), tty_clear, menuMedico(IDM);
                                write('Opção Inválida\n'), menuMedicoAcessarExames(IDM)).

menuMedicoAcessarExamesEspecifico(IDM) :- write('ID do (E)xame'), nl,
                                          /*medico:validaIDExame(IdExame)*/
                                          write('ID do (P)aciente'), nl,
                                          paciente:validaIDPaciente(ID),
                                          promptString('Opção > ', O),
                                          (O = "E" -> medico:acessarExame(IdExame), tty_clear, menuMedico(IDM);
                                          O = "P" -> medico:acessarExame(IdExame), tty_clear, menuMedico(IDM);
                                          write('ID informado é inválido!\n'), menuMedicoAcessarExamesEspecifico(IDM)).

menuMedicoAcessarAgendamentos(IDM) :- write('(T)odos'), nl,
                                      write('(E)specíficos'), nl,
                                      promptString('Opção > ', O),
                                      (O = "T" -> medico:acessarConsultas(IDM), tty_clear, menuMedico(IDM);
                                      O = "E" -> menuMedicoAcessarExamesAgendamentosEspecifico(IDM), tty_clear, menuMedico(IDM);
                                      write('Opção Inválida\n'), menuMedicoAcessarAgendamentos(IDM)).                  

menuMedicoAcessarAgendamentosEspecifico(IDM) :- write('Data > '), nl,
                                                promptString('Data > ', Data),
                                                (Data -> medico:acessarConsultasData), tty_clear, menuMedico(IDM);
                                                write('Data Inválida\n'), menuMedicoAcessarAgendamentosEspecifico(IDM).

menuMedicoEmitir(IDM) :- write('(R)eceita'), nl,
                         write('Resultado de (E)xame'), nl,
                         write('(L)audo Médico'), nl,
                         promptString('Opção > ', O),
                         (O = "R" -> menuMedicoEmitirReceita(IDM), tty_clear, menuMedico(IDM);
                         O = "E" -> menuMedicoEmitirResultado(IDM), tty_clear, menuMedico(IDM);
                         O = "L" -> menuMedicoEmitirLaudo(IDM), tty_clear, menuMedico(IDM);
                         write('Opção Inválida\n'), menuMedicoEmitir(IDM)).
                                                      
menuMedicoEmitirReceita(IDM) :- prompt('ID do Paciente > ', ID),
                                paciente:validaIDPaciente(ID),
                                medico:emitirReceitas, tty_clear, menuMedico(ID),
                                write('ID informado é inválido!\n'), menuMedicoEmitirReceita(IDM).

menuMedicoEmitirResultado(IDM) :- prompt('ID do Exame > ', IdExame),
                                  /*medico:validaIDExame(IdExame)*/
                                  medico:emitirResultadoExame, tty_clear, menuMedico(IDM),
                                  /*format('Resultado > ')*/
                                  write('ID informado é inválido!\n'), menuMedicoEmitirResultado(IDM).

menuMedicoEmitirLaudo(IDM) :- prompt('ID do Exame > ', IdExame),
                              /*medico:validaIDExame(IdExame)*/
                              medico:emitirLaudo, tty_clear, menuMedico(IDM),
                              /*format('Resultado > ')*/
                              write('ID informado é inválido!\n'), menuMedicoEmitirLaudo(IDM).   

menuMedicoTransferencia(IDM) :- prompt('ID da UBS > ', IdUBS),
                                ubs:validaIDUBS(IdUBS),
                                /*medico:validaIDExame(IdExame)*/
                                medico:solicitarTransferencia, tty_clear, menuMedico(IDM),
                                /*format('Resultado > ')*/
                                write('ID informado é inválido!\n'), menuMedicoEmitirLaudo(IDM).
