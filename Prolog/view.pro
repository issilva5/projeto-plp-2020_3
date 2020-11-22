:- module(view, [main/0, login/0, cadastro/0, menuPaciente/0, menuUBS/1, menuMedico/0]).

:- use_module('utils.pro').
:- use_module('model.pro').
:- use_module('ubsController.pro').
:- use_module('medicoController.pro').

/* Menu inicial. */
main.

/* Menu de login. */
login :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - LOGIN  \n
-----------------------------------------------------------------\n'),
         prompt('Insira seu ID > ', ID),
         promptString('Insira sua senha > ', S), autentica(ID, S, T),
         (T =:= 0 -> menuPaciente ;
          T =:= 1 -> menuUBS(ID);
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
               assertz(model:logins(N, Senha, 0)),
               write('Cadastrado de paciente realizado com sucesso, id: '), write(N).

/* Lê as opções de cadastro da UBS e faz seu cadastro. */
cadastraUBS :- promptString('Nome > ', Nome),
               promptString('Endereço > ', Endereco),
               promptString('Senha > ', Senha),
               model:nextId(N),
               assertz(model:ubs(N, Nome, Endereco)),
               assertz(model:logins(N, Senha, 1)),
               write('Cadastrado de UBS realizado com sucesso, id: '), write(N).     

/* Menu do paciente. */
menuPaciente.

/* Menu da UBS. */
menuUBS(IdUBS) :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - UBS  \n
    -----------------------------------------------------------------\n'),
    write('(C)adastrar corpo médico'), nl, write('(V)isualizar informações'), nl,
    write('(F)armácia'), nl, write('(D)ashboard'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> cadastraMedico(IdUBS), tty_clear, main ;
     Op = "V" -> visualizaUBS(IdUBS), tty_clear, main ;
     Op = "F" -> farmaciaUBS(IdUBS), tty_clear, main ;
     Op = "D" -> dashBoard(IdUBS), tty_clear, main;
     write('Opção inválida\n'), menuUBS(IdUBS)).

cadastraMedico(IdUBS) :- promptString('Nome > ', Nome),
    promptString('CRM > ', CRM),
    promptString('Especialidade > ', Especialidade),
    promptString('Senha > ', Senha),
    model:nextId(N),
    assertz(model:medico(N, Nome, CRM, IdUBS, Especialidade)),
    assertz(model:logins(N, Senha, 2)),
    write('Cadastrado de médico realizado com sucesso, id: '), write(N).

visualizaUBS(IdUBS) :- write('(A)gendamentos'), nl, write('(P)aciente'), nl, write('(M)édico'), nl,
    promptString('Opção > ', Op),
    (Op = "A" -> ubs:visualizaConsultasFuturas(IdUBS), tty_clear, main;
    Op = "P" -> ubs:visualizaPacientes(IdUBS), tty_clear, main;
    Op = "M" -> visualizaUBSMedicos(IdUBS), tty_clear, main;
    write('Opção inválida\n'), menuUBS(IdUBS)).

visualizaUBSMedicos(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:visualizaMedicos(IdUBS), tty_clear, main;
    Op = "E" -> promptString('Id Médico > ', IdMed), medico:validaIDMedico(IdUBS, IdMed),ubs:visualizaMedico(IdUBS, IdMed), tty_clear, main;
    write('Opção inválida\n'), menuUBS(IdUBS)).

farmaciaUBS(IdUBS) :- write('(C)onsultar'), nl, write('(N)ovo'), nl, write('(A)dicionar'), nl, write('(R)emover'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> farmaciaUBSConsultar(IdUBS), tty_clear, main;
    Op = "N" -> novoMedicamento(IdUBS), tty_clear, main;
    Op = "A" -> adicionaMedicamentoEstoque(IdUBS), tty_clear, main;
    Op = "R" -> removeMedicamentoEstoque(IdUBS), tty_clear, main;
    write('Opção inválida\n'), menuUBS(IdUBS)).

farmaciaUBSConsultar(IdUBS) :- write('(T)odos'), nl, write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:consultarMedicamentos(IdUBS), tty_clear, main;
    Op = "E" -> promptString('Id Medicamento > ', IdMed), ubs:validaIDMedicamento(IdUBS, IdMed),ubs:consultarMedicamento(IdUBS, IdMed), tty_clear, main;
    write('Opção inválida\n'), menuUBS(IdUBS)).

novoMedicamento(IdUBS) :- promptString('Nome > ', Nome),
    promptString('Nome > ', Nome),
    promptString('Quantidade > ', Quantidade),
    promptString('Bula > ', Bula),
    model:nextId(N),
    assertz(model:medicamento(N, IdUBS, Nome, Quantidade, Bula)),
    write('Cadastrado de medicamento realizado com sucesso, id: '), write(N).

adicionaMedicamentoEstoque(IdUBS) :- promptString('Id Medicamento > ', IdMed), 
    promptString('Quantidade a adicionar > ', Qtd),
    ubs:validaIDMedicamento(IdUBS, IdMed), ubs:adicionaMedicamentoEstoque(IdUBS, IdMed, Qtd).

removeMedicamentoEstoque(IdUBS) :- promptString('Id Medicamento > ', IdMed), 
    promptString('Quantidade a remover > ', Qtd),
    ubs:validaIDMedicamento(IdUBS, IdMed), ubs:removeMedicamentoEstoque(IdUBS, IdMed, Qtd).

dashBoard(_).

/* Menu do medico. */
menuMedico.