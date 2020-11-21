:- encoding(utf8).

:- module(view, [main/0, login/0, cadastro/0, menuPaciente/0, menuUBS/0, menuMedico/0]).

:-use_module(library(tty)).

:- use_module('./Utils/utils.pro').
:- use_module('./Models/model.pro').

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
                (O = "B" -> menuPacienteBuscar(ID), tty_clear, menuPaciente(ID);
                O = "R" -> menuPacienteRequisitar(ID), tty_clear, menuPaciente(ID);
                O = "C" -> menuPacienteConsultar(ID), tty_clear, menuPaciente(ID);
                O = "E" -> menuPacienteEmergencia, tty_clear, menuPaciente(ID);
                O = "S" -> tty_clear, login;
                write('Opcao Invalida\n'), menuPaciente(ID)).

menuPacienteBuscar(ID) :- promptString('Especialidade > ', E).

menuPacienteRequisitar(ID) :- write('(C)onsulta'), nl,
                              write('(E)xame'), nl,
                              write('(M)edicamento'), nl,
                              promptString('Opção > ', O),
                              (O ="C" -> leituraRequisitaConsulta(ID), tty_clear, menuPaciente(ID);
                              O = "E" -> leituraRequisitaExame(ID), tty_clear, menuPaciente(ID);
                              O = "M" -> leituraRequisitaMedicamento(ID), tty_clear, menuPaciente(ID);
                              write('Opção Inválida\n'), menuPacienteRequisitar(ID)).

% falta a validação e a chamada para o metodo do controller!
leituraRequisitaConsulta(ID) :- promptString('ID do Médico > ', IDM),
                                promptString('ID da UBS > ', IDU).

leituraRequisitaExame(ID) :- promptString('ID do Médico > ', IDM),
                             promptString('ID da UBS > ', IDU),
                             promptString('Tipo de Exame > ', TIPO).

leituraRequisitaMedicamento(ID) :- promptString('ID da Receita > ', IDR).

menuPacienteConsultar(ID) :- write('(L)audo'), nl,
                             write('(R)eceita'), nl,
                             write('(C)onsultas'), nl,
                             promptString('Opção > ', O),
                             (O = "L" -> leituraConsultaLaudo(ID), tty_clear, menuPaciente(ID);
                             O = "R" -> leituraConsultaReceita(ID), tty_clear, menuPaciente(ID);
                             O = "C" -> leituraConsultaConsultas(ID), tty_clear, menuPaciente(ID);
                             write('Opção Inválida\n'), menuPacienteConsultar(ID)).

leituraConsultaLaudo(ID) :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosLaudos(ID);
                            O = "E" -> leituraConsultaLaudoEspecifico(ID);
                            write('Opção Inválida\n'), leituraConsultaLaudo(ID)).

leituraConsultaTodosLaudos(ID).
leituraConsultaLaudoEspecifico(ID) :- promptString('ID do Laudo > ', IDL).

leituraConsultaReceita(ID) :- write('(M)edicamento'), nl,
                              write('(E)xame'), nl,
                              promptString('Opção > ', O),
                              (O = "M" -> leituraConsultaMedicamento(ID);
                              O = "E" -> leituraConsultaExame(ID);
                              write('Opção Inválida\n'), leituraConsultaReceita(ID)).

leituraConsultaMedicamento(ID) :- write('(T)odos'), nl,
                                  write('(E)specífico'), nl,
                                  promptString('Opção > ', O),
                                  (O = "T" -> leituraConsultaTodasReceitaMedicamento(ID);
                                  O = "E" -> leituraConsultaReceitaMedicamentoEspecifico(ID),
                                  write('Opção Inválida\n'), leituraConsultaMedicamento(ID)).

leituraConsultaTodasReceitaMedicamento(ID).
leituraConsultaReceitaMedicamentoEspecifico(ID) :- promptString('ID da Receita > ', IDR).

leituraConsultaExame(ID) :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosExames(ID);
                            O = "E" -> leituraConsultaExameEspecifico(ID),
                            write('Opção Inválida\n'), leituraConsultaExame(ID)).

leituraConsultaTodosExames(ID).
leituraConsultaExameEspecifico(ID) :- promptString('ID da Receita > ', IDR).

leituraConsultaConsultas(ID) :- write('IMPRIMIR TODAS AS CONSULTAS').

menuPacienteEmergencia :- promptString('Endereço > ', E),
                          ansi_format([bold, fg(green)], 'Uma ambulância está a caminho!', []), nl,
                          ansi_format([bold, fg(green)], 'wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo wiii-wooo-wiii-wooo', []).

/* Menu da UBS. */
menuUBS.

/* Menu do medico. */
menuMedico.