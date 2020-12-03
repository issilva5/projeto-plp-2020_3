:- encoding(utf8).

%
:- use_module('./Utils/utils.pro').
:- use_module('./Utils/time.pro').
:- use_module('./Models/model.pro').
:- use_module('./Controllers/pacienteController.pro').
:- use_module('./Controllers/medicoController.pro').
:- use_module('./Controllers/ubsController.pro').
:- use_module('./Persistence/persistence.pro').

begin :- model:iniciaSistema,
         main.

/* Menu inicial. */
main :-
    writeln( '-----------------------------------------------------------------'),
    writeln( '         SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS)         '),
    writeln( '-----------------------------------------------------------------'),
    writeln('(L)ogin     '),
    writeln('(C)adastrar '),
    writeln('(E)ncerrar  '),
    promptString('Opção > ', OP),
    ( OP = "L" -> tty_clear, login;
      OP = "C" -> tty_clear, cadastro;
      OP = "E" -> writeln('Tchau');
      writeln('Opção Inválida'), utils:mensagemEspera, tty_clear, main).

/* Menu de login. */
login :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - LOGIN  \n
-----------------------------------------------------------------\n'),
         prompt('Insira seu ID > ', ID),
         promptString('Insira sua senha > ', S), autentica(ID, S, T),
         (T =:= 0 -> tty_clear, menuPaciente(ID);
          T =:= 1 -> tty_clear, menuUBS(ID);
          T =:= 2 -> tty_clear, menuMedico(ID) ;
          write('ID não encontrado'), utils:mensagemEspera, main).

/* Menu de cadastro. */
cadastro :- write('-----------------------------------------------------------------\n
   SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - CADASTRO  \n
-----------------------------------------------------------------\n'),
    write('(P)aciente'), nl, write('(U)BS'), nl, write('(V)oltar'), nl,
    promptString('Opção > ', O),
    (O = "P" -> cadastraPac, main;
     O = "U" -> cadastraUBS, main;
     O = "V" -> tty_clear, main;
     write('Opção inválida'), utils:mensagemEspera, cadastro).

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
               persistence:escreveId,
               persistence:escrevePaciente,
               persistence:escreveLogins,
               format('\nCadastro de paciente realizado com sucesso, id: ~d', [N]),
               utils:mensagemEspera.

/* Lê as opções de cadastro da UBS e faz seu cadastro. */
cadastraUBS :- promptString('Nome > ', Nome),
               promptString('Endereço > ', Endereco),
               promptString('Senha > ', Senha),
               model:nextId(N),
               assertz(model:ubs(N, Nome, Endereco)),
               assertz(model:logins(N, Senha, 1)),
               persistence:escreveId,
               persistence:escreveUBS,
               persistence:escreveLogins,
               format('\nCadastro de UBS realizado com sucesso, id: ~d', [N]),
               utils:mensagemEspera.

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
                (O = "B" -> menuPacienteBuscarUBS(ID), utils:mensagemEspera, tty_clear, menuPaciente(ID);
                O = "R" -> menuPacienteRequisitar(ID), utils:mensagemEspera, tty_clear, menuPaciente(ID);
                O = "C" -> menuPacienteConsultar(ID), utils:mensagemEspera, tty_clear, menuPaciente(ID);
                O = "E" -> menuPacienteEmergencia, utils:mensagemEspera, tty_clear, menuPaciente(ID);
                O = "S" -> tty_clear, main;
                write('Opção Inválida'), utils:mensagemEspera, tty_clear, menuPaciente(ID)).

menuPacienteBuscarUBS(ID) :- write('(T)odas as UBS'), nl,
                            write('(E)specilidades da UBS'), nl,
                            write('(U)BS por especialidade'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> menuPacienteBuscarTodasUbs;
                            O = "E" -> menuPacienteBuscarEspecialidadesUbs(ID);
                            O = "U" -> menuPacienteBuscarUBSPorEspecialidade;
                            write('Opção Inválida'), utils:mensagemEspera, menuPaciente(ID)).

menuPacienteBuscarUBSPorEspecialidade :- promptString('Especialidade > ', E), paciente:buscarUnidadesEspec(E).

menuPacienteBuscarTodasUbs :- paciente:buscarTodasUnidades.

menuPacienteBuscarEspecialidadesUbs(ID) :- prompt('Id da UBS > ', I), 
                                        (ubs:validaIDUBS(I) -> paciente:especialidadeDaUBS(I);
                                        write('Id inválido\n'), menuPacienteBuscarUBS(ID)).

menuPacienteRequisitar(ID) :- write('(C)onsulta'), nl,
                              write('(E)xame'), nl,
                              write('(M)edicamento'), nl,
                              promptString('Opção > ', O),
                              (O ="C" -> leituraRequisitaConsulta(ID);
                              O = "E" -> leituraRequisitaExame(ID);
                              O = "M" -> leituraRequisitaMedicamento(ID);
                              write('Opção Inválida\n'), menuPacienteRequisitar(ID)).

leituraRequisitaConsulta(ID) :- prompt('ID do Médico > ', IDM),
                                medico:validaIDMedico(IDM),
                                medico:pegaUBS(IDM, IDU),
                                time:getNextDate(IDM, Data),
                                model:nextId(N),
                                persistence:escreveId,
                                paciente:requisitarConsulta(N, ID, IDM, IDU, Data),
                                persistence:escreveConsulta,
                                format('Consulta ~d Marcada com o Médico ~d, na UBS ~d e no(a) ', [N, IDM, IDU]),
                                format_time(user, '%a, %d %b %Y %T', Data),
                                format('.~n').       

leituraRequisitaExame(ID) :- prompt('ID do Médico > ', IDM),
                             medico:validaIDMedico(IDM),
                             medico:pegaUBS(IDM, IDU),
                             promptString('Tipo de Exame > ', Tipo),
                             time:getNextDate(IDM, Data),
                             model:nextId(N),
                             persistence:escreveId,
                             paciente:requisitarExame(N, ID, IDM, IDU, Tipo, Data),
                             persistence:escreveExame,
                             format('Exame ~d marcado na UBS ~d e no(a) ', [N, IDU]),
                             format_time(user, '%a, %d %b %Y %T', Data),
                             format('.~n').

leituraRequisitaMedicamento(ID) :- prompt('ID da Receita > ', IDR),
                                   (ubs:validaIDReceita(IDR) -> paciente:requisitarMedicamento(IDR, ID), persistence:escreveMedicamento;
                                   write('Id inválido\n'), menuPacienteRequisitar(ID)).

menuPacienteConsultar(ID) :- write('(L)audo'), nl,
                             write('(R)eceita'), nl,
                             write('(E)xame'), nl,
                             write('(C)onsultas'), nl,
                             promptString('Opção > ', O),
                             (O = "L" -> leituraConsultaLaudo(ID);
                             O = "R" -> leituraConsultaReceita(ID);
                             O = "E" -> leituraConsultaExame(ID);
                             O = "C" -> leituraConsultaConsultas(ID);
                             write('Opção Inválida\n'), menuPacienteConsultar(ID)).

leituraConsultaLaudo(ID) :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosLaudos(ID);
                            O = "E" -> leituraConsultaLaudoEspecifico(ID);
                            write('Opção Inválida\n'), leituraConsultaLaudo(ID)).

leituraConsultaTodosLaudos(ID) :- paciente:consultarLaudos(ID).

leituraConsultaLaudoEspecifico(ID) :- prompt('ID do Laudo > ', IDL),
                                      (ubs:validaIDLaudo(IDL) -> paciente:consultarLaudo(IDL);
                                      write('Id inválido\n'), leituraConsultaLaudo(ID)).

leituraConsultaReceita(ID) :- write('(T)odas'), nl,
                              write('(E)specífica'), nl,
                              promptString('Opção > ', O),
                              (O = "T" -> leituraConsultaTodasReceitas(ID);
                              O = "E" -> leituraConsultaReceitaEspecifica(ID);
                              write('Opção Inválida\n'), leituraConsultaReceita(ID)).

leituraConsultaTodasReceitas(ID) :- paciente:consultarReceitas(ID).

leituraConsultaReceitaEspecifica(ID) :- prompt('ID da Receita > ', IDR),
                                        (ubs:validaIDReceita(IDR) -> paciente:consultarReceita(ID, IDR);
                                        write('Id inválido\n'), leituraConsultaReceita(ID)).

leituraConsultaExame(ID) :- write('(T)odos'), nl,
                            write('(E)specífico'), nl,
                            promptString('Opção > ', O),
                            (O = "T" -> leituraConsultaTodosExames(ID);
                            O = "E" -> leituraConsultaExameEspecifico(ID);
                            write('Opção Inválida\n'), leituraConsultaExame(ID)).

leituraConsultaTodosExames(ID) :- paciente:consultarExames(ID).

leituraConsultaExameEspecifico(ID) :- prompt('ID do Exame > ', IdEx),
                                      (ubs:validaIDExame(IdEx) -> paciente:consultarExame(ID, IdEx);
                                      write('Id inválido\n'), leituraConsultaExame(ID)).

leituraConsultaConsultas(ID) :- write('(T)odas'), nl,
                                write('(E)specífica'), nl,
                                promptString('Opção > ', O),
                                (O = "T" -> leituraConsultaTodasConsultas(ID);
                                O = "E" -> leituraConsultaConsultaEspecifica(ID);
                                write('Opção Inválida\n'), leituraConsultaConsultas(ID)).

leituraConsultaTodasConsultas(ID) :- paciente:consultarConsultas(ID).

leituraConsultaConsultaEspecifica(ID) :- prompt('ID da Consulta > ', IDC),
                                     (ubs:validaIDConsulta(IDC) -> paciente:consultarConsulta(ID, IDC);
                                     write('Id inválido\n'), leituraConsultaConsultas(ID)).

menuPacienteEmergencia :- promptString('Endereço > ', E),
                          paciente:emergencia(E).

/* Menu da UBS. */
menuUBS(IdUBS) :- write('-----------------------------------------------------------------\n
    SISTEMA INTEGRADO DE ASSISTÊNCIA À SAÚDE (SIAS) - UBS  \n
-----------------------------------------------------------------\n'),
    write('(C)adastrar corpo médico'), nl,
    write('(V)isualizar informações'), nl,
    write('(F)armácia'), nl,
    write('(D)ashboard'), nl,
    write('(S)air'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> cadastraMedico(IdUBS), utils:mensagemEspera, menuUBS(IdUBS);
     Op = "V" -> visualizaUBS(IdUBS), utils:mensagemEspera, menuUBS(IdUBS);
     Op = "F" -> farmaciaUBS(IdUBS), utils:mensagemEspera, menuUBS(IdUBS);
     Op = "D" -> dashBoard(IdUBS), utils:mensagemEspera, menuUBS(IdUBS);
     Op = "S" -> tty_clear, main;
     write('Opção inválida'), utils:mensagemEspera, menuUBS(IdUBS)).

cadastraMedico(IdUBS) :- ubs:cadastraMedico(IdUBS).

visualizaUBS(IdUBS) :- write('(A)gendamentos'), nl,
    write('(P)aciente'), nl,
    write('(M)édico'), nl,
    promptString('Opção > ', Op),
    (Op = "A" -> ubs:visualizaConsultasFuturas(IdUBS);
    Op = "P" -> ubs:visualizaPacientes(IdUBS);
    Op = "M" -> visualizaUBSMedicos(IdUBS);
    write('Opção inválida')).

visualizaUBSMedicos(IdUBS) :- write('(T)odos'), nl,
    write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:visualizaMedicos(IdUBS);
    Op = "E" -> visualizaUBSMedico(IdUBS);
    write('Opção inválida')).

visualizaUBSMedico(IdUBS) :- prompt('Id Médico > ', IdMed), 
    (medico:validaIDMedico(IdMed) -> ubs:visualizaMedico(IdMed, IdUBS);
    write('Id inválido')).

farmaciaUBS(IdUBS) :- write('(C)onsultar'), nl,
    write('(N)ovo'), nl,
    write('(A)dicionar'), nl,
    write('(R)emover'), nl,
    promptString('Opção > ', Op),
    (Op = "C" -> farmaciaUBSConsultar(IdUBS);
    Op = "N" -> novoMedicamento(IdUBS);
    Op = "A" -> adicionaMedicamentoEstoque(IdUBS);
    Op = "R" -> removeMedicamentoEstoque(IdUBS);
    write('Opção inválida')).

farmaciaUBSConsultar(IdUBS) :- write('(T)odos'), nl,
    write('(E)specífico'), nl,
    promptString('Opção > ', Op),
    (Op = "T" -> ubs:consultarMedicamentos(IdUBS) ;
    Op = "E" -> (prompt('Id Medicamento > ', IdMed), 
        ubs:validaIDMedicamento(IdMed),
        ubs:consultarMedicamento(IdMed, IdUBS) ; write('ID inválido')) ;
    write('Opção inválida')).

novoMedicamento(IdUBS) :- promptString('Nome > ', Nome),
    prompt('Quantidade > ', Qtd),
    promptString('Bula > ', Bula),
    model:nextId(N),
    persistence:escreveId,
    assertz(model:medicamento(N, IdUBS, Nome, Qtd, Bula)),
    persistence:escreveMedicamento,
    show:showMedicamento(model:medicamento(N, IdUBS, Nome, Qtd, Bula)).

adicionaMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a adicionar > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:adicionaMedicamentoEstoque(IdMed, IdUBS, Qtd) ;
    write('Id inválido')).

removeMedicamentoEstoque(IdUBS) :- prompt('Id Medicamento > ', IdMed), 
    prompt('Quantidade a remover > ', Qtd),
    (ubs:validaIDMedicamento(IdMed) -> ubs:removeMedicamentoEstoque(IdMed, IdUBS, Qtd) ;
    write('Id inválido')).

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
                write('Opção Inválida'), mensagemEspera, menuMedico(IDM)).

menuMedicoAcessarDados(IDM) :- write('(P)acientes'), nl,
                               write('(E)xames'), nl,
                               write('(C)onsultas agendadas'), nl,
                               write('(M)edicamentos'), nl,
                               promptString('Opção > ', O),
                               (O = "P" -> acessarDadosPacientes ;
                               O = "E" -> menuMedicoAcessarExames(IDM) ;
                               O = "C" -> menuMedicoAcessarAgendamentos(IDM) ;
                               O = "M" -> menuMedicoAcessarMedicamentos ;
                               write('Opção Inválida')).

acessarDadosPacientes :- prompt('ID do Paciente > ', ID),
                         (paciente:validaIDPaciente(ID) -> medico:acessarDadosPaciente(ID) ; write('ID inválido!')).
 
menuMedicoAcessarExames(IDM) :- write('(T)odos'), nl,
                                write('(E)specíficos'), nl,
                                promptString('Opção > ', O),
                                (O = "T" -> medico:acessarExames(IDM, 0) ;
                                O = "E" -> menuMedicoAcessarExamesEspecifico ;
                                write('Opção Inválida')).

menuMedicoAcessarExamesEspecifico :- write('ID do (E)xame'), nl,
                                    write('ID do (P)aciente'), nl,
                                    promptString('Opção > ', O),
                                    prompt('Insira o ID > ', ID),
                                    ((O = "E", ubs:validaIDExame(ID)) -> medico:acessarExames(ID, 1);
                                    (O = "P", paciente:validaIDPaciente(ID)) -> medico:acessarExames(ID, 2);
                                    write('Opção ou ID informado é inválido!')).


menuMedicoAcessarAgendamentos(IDM) :- write('(T)odos'), nl,
                                      write('(E)specíficos'), nl,
                                      promptString('Opção > ', O),
                                      (O = "T" -> medico:acessarConsultas(IDM, 0);
                                      O = "E" -> menuMedicoAcessarAgendamentosEspecifico(IDM);
                                      write('Opção Inválida')).                  

menuMedicoAcessarAgendamentosEspecifico(IDM) :- write('Data > '), nl,
                                                promptString('Data > ', Data),
                                                (medico:acessarConsultas(IDM, Data) ; write('Data Inválida')).

menuMedicoAcessarMedicamentos :- write('(T)odos'), nl,
                                      write('(E)specíficos'), nl,
                                      promptString('Opção > ', O),
                                      (O = "T" -> medico:acessarMedicamentos ;
                                      O = "E" -> prompt('Insira o ID > ', ID), medico:acessarMedicamentos(ID) ;
                                      write('Opção Inválida')).

menuMedicoEmitir(IDM) :- write('(R)eceita'), nl,
                         write('Resultado de (E)xame'), nl,
                         write('(L)audo Médico'), nl,
                         promptString('Opção > ', O),
                         (O = "R" -> menuMedicoEmitirReceita(IDM);
                         O = "E" -> menuMedicoEmitirResultado;
                         O = "L" -> menuMedicoEmitirLaudo(IDM);
                         write('Opção Inválida')).
                                                      
menuMedicoEmitirReceita(IDM) :- ((prompt('ID do Paciente > ', ID), paciente:validaIDPaciente(ID)) ->  medico:emitirReceita(IDM, ID)) ; write('ID inválido!').

menuMedicoEmitirResultado :- ((prompt('ID do Exame > ', IdExame), ubs:validaIDExame(IdExame)) -> medico:emitirResultadoExame(IdExame)) ; write('ID inválido!').

menuMedicoEmitirLaudo(IDM) :- ((prompt('ID do Exame > ', IdExame), ubs:validaIDExame(IdExame)) -> medico:emitirLaudo(IDM, IdExame)) ; write('ID inválido!').   

menuMedicoTransferencia(IDM) :- prompt('ID da UBS de destino > ', IdUBS),
                                ubs:validaIDUBS(IdUBS), medico:solicitarTransferencia(IDM, IdUBS) ; write('ID inválido!').
