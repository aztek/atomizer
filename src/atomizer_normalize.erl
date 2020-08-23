-module(atomizer_normalize).

-export([normalize/1]).

-define(IS_ALPHA_LOWER(C), C >= $a, C =< $z).
-define(IS_ALPHA_UPPER(C), C >= $A, C =< $Z).
-define(IS_DIGIT(C),       C >= $0, C =< $9).
-define(IS_DELIMITER(C),   C == $_; C == $-).

-spec normalize(atom()) -> {ok, atomizer:normal_form()} | nok.
%% Normalize atom names spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination.
normalize(Atom) ->
    case string:lowercase([C || C <- atomizer:pretty_atom(Atom), C /= $_, C /= $-]) of
        [] -> nok;
        NormalForm -> {ok, list_to_atom(NormalForm)}
    end.
