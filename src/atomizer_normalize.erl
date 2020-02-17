-module(atomizer_normalize).

-export([normalize/1]).

-include("atomizer.hrl").

-define(IS_ALPHA_LOWER(C), C >= $a, C =< $z).
-define(IS_ALPHA_UPPER(C), C >= $A, C =< $Z).
-define(IS_DIGIT(C),       C >= $0, C =< $9).
-define(IS_DELIMITER(C),   C == $_; C == $-).

-spec normalize(atom()) -> [normal_form()].
%% Normalize atom names spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination.
normalize(Atom) ->
    String = hd(io_lib:format("~p", [Atom])),
    case string:lowercase([C || C <- String, C /= $_, C /= $-]) of
        [] -> [];
        NormalForm -> [list_to_atom(NormalForm)]
    end.
