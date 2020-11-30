:- module(utils, [prompt/2, promptString/2, autentica/3, mensagemEspera/0]). %% na lista devem entrar as regras publicas do módulo na forma <nome>/<n>

:- use_module('../Models/model.pro').


/*

Lê um valor númerico da entrada padrão.
prompt(+Text, -Value).

*/
prompt(Text, Value) :- promptString(Text, V), atom_number(V, Value).

/*

Lê uma linha da entrada padrão.
promptString(+Text, -Value).

*/
promptString(Text, Value) :- read_pending_chars(user_input, _, _),
                             write(Text), flush_output(user),
                             read_line_to_string(user_input, Value).

/*

Verifica se um login existe.
autentica(+ID, +Senha, -Tipo).

*/
autentica(ID, Senha, Tipo) :- model:logins(ID, Senha, Tipo), !.
autentica(_, _, Tipo) :- Tipo is -1.

/*

Imprime mensagem e para a execução até receba uma entrada.

*/

mensagemEspera :- promptString('\n\nPressione qualquer tecla para continuar', _), tty_clear.