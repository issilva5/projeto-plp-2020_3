:- encoding(utf8).

:- module(view, [main/0, login/0, cadastro/0, menuPaciente/1, menuUBS/0, menuMedico/0]).

:- use_module('./Utils/utils.pro').
:- use_module('./Models/model.pro').
:- use_module('./Controllers/pacienteController.pro').
:- use_module('./Controllers/medicoController.pro').
:- use_module('./Controllers/ubsController.pro').

/* Menu inicial. */
main.

/* Menu de login. */
login :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - LOGIN  \n
-----------------------------------------------------------------\n'),
         prompt('Insira seu ID > ', ID),
         promptString('Insira sua senha > ', S), autentica(ID, S, T),
         (T =:= 0 -> menuPaciente(ID) ;
          T =:= 1 -> menuUBS ;
          T =:= 2 -> menuMedico ;
          write('ID não encontrado\n'), login).

/* Menu de cadastro. */
cadastro :- write('-----------------------------------------------------------------\n
   SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - CADASTRO  \n
-----------------------------------------------------------------\n'),
    write('(P)aciente'), nl, write('(U)BS'), nl, write('(V)oltar'), nl,
    promptString('Opção > ', O),
    (O = "P" -> cadastraPac, tty_clear, main ;
     O = "U" -> cadastraUBS, tty_clear, main ;
     O = "V" -> tty_clear, main ;
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
               assertz(model:logins(N, Senha, 0)).

/* Lê as opções de cadastro da UBS e faz seu cadastro. */
cadastraUBS :- promptString('Nome > ', Nome),
               promptString('Endereço > ', Endereco),
               promptString('Senha > ', Senha),
               model:nextId(N),
               assertz(model:ubs(N, Nome, Endereco)),
               assertz(model:logins(N, Senha, 1)).

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
                O = "S" -> tty_clear, login;
                write('Opcao Invalida\n'), menuPaciente(ID)).

menuPacienteBuscarUBSPorEspecialidade :- promptString('Especialidade > ', E),
                                        forall(paciente:buscarUnidadesEspec(E, I, U, End), format('~w, ~w ~n', [U, End])).

menuPacienteRequisitar(ID) :- write('(C)onsulta'), nl,
                              write('(E)xame'), nl,
                              write('(M)edicamento'), nl,
                              promptString('Opção > ', O),
                              (O ="C" -> leituraRequisitaConsulta(ID), tty_clear, menuPaciente(ID);
                              O = "E" -> leituraRequisitaExame(ID), tty_clear, menuPaciente(ID);
                              O = "M" -> leituraRequisitaMedicamento(ID), tty_clear, menuPaciente(ID);
                              write('Opção Inválida\n'), menuPacienteRequisitar(ID)).

leituraRequisitaConsulta(ID) :- promptString('ID do Médico > ', IDM),
                                medico:validaIDMedico(IDM),
                                promptString('ID da UBS > ', IDU),
                                ubs:validaIDUBS(IDU),
                                promptString('Informe a data desejada: ', Data),
                                model:nextId(N),
                                paciente:requisitarConsulta(N, ID, IDM, IDU, Data),
                                format('Consulta ~d Marcada com o Médico ~d, na UBS ~d e no dia ~w ~n', [N, IDM, IDU, Data]).

leituraRequisitaExame(ID) :- promptString('ID do Médico > ', IDM),
                             medico:validaIDMedico(IDM),
                             promptString('ID da UBS > ', IDU),
                             ubs:validaIDUBS(IDU),
                             promptString('Tipo de Exame > ', Tipo),
                             promptString('Informe a data desejada: ', Data),
                             model:nextId(N),
                             paciente:requisitarExame(N, ID, IDM, IDU, TIPO, Data),
                             format('Exame ~d marcado na UBS ~d e no dia ~w ~n', [N, IDU, Data]).

leituraRequisitaMedicamento(ID) :- promptString('ID da Receita > ', IDR),
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

leituraConsultaLaudoEspecifico(ID) :- promptString('ID do Laudo > ', IDL),
                                      ubs:validaIDLaudo(IDL),
                                      paciente:consultaLaudo(ID, IDL, IDM, IDU, Dia),
                                      format('Laudo ~d: ~d, ~d, ~w', [IDL, IDM, IDU, Dia])).

leituraConsultaReceita :- write('(T)odas'), nl,
                              write('(E)specífica'), nl,
                              promptString('Opção > ', O),
                              (O = "T" -> leituraConsultaTodasReceitas;
                              O = "E" -> leituraConsultaReceitaEspecifica;
                              write('Opção Inválida\n'), leituraConsultaReceita.

leituraConsultaTodasReceitas :- forall(paciente:consultarReceitas(IDR, IDM, IDU), 
                                    format('Receita ~d: ~d, ~d, ~w', [IDR, IDM, IDU])).

leituraConsultaReceitaEspecifica :- promptString('ID da Receita > ', IDR),
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

leituraConsultaExameEspecifico :- promptString('ID do Exame > ', IDE),
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

leituraConsultaConsultaEspecifica :- promptString('ID da Consulta > ', IDC),
                                     paciente:consultarConsulta(IDC, IDM, IDU, Dia), 
                                     format('Consulta ~d: ~d, ~d, ~w', [IDC, IDM, IDU, Dia]).

menuPacienteEmergencia :- promptString('Endereço > ', E),
                          paciente:emergencia(E).

/* Menu da UBS. */
menuUBS.

/* Menu do medico. */
menuMedico.