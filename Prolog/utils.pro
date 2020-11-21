:- module(utils, [prompt/2, promptString/2, autentica/3]). %% na lista devem entrar as regras publicas do módulo na forma <nome>/<n>

:- use_module('model.pro').


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