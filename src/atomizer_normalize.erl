-module(atomizer_normalize).

-include_lib("eunit/include/eunit.hrl").

-export([normalize/1]).

-define(IS_ALPHA_LOWER(C), C >= $a, C =< $z).
-define(IS_ALPHA_UPPER(C), C >= $A, C =< $Z).
-define(IS_DIGIT(C),       C >= $0, C =< $9).
-define(IS_DELIMITER(C),   C == $_; C == $-).

-spec normalize(atom()) -> [atom()].
%% Normalize atom named spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination.
normalize(Atom) ->
    Split = fun (C, Accs) when ?IS_DELIMITER(C) -> [[] | Accs];
                (C, [[C0 | Acc] | Accs]) when ?IS_ALPHA_UPPER(C), ?IS_ALPHA_UPPER(C0) -> [[C, C0 | Acc] | Accs];
                (C, Accs) when ?IS_ALPHA_UPPER(C) -> [[C] | Accs];
                (C, [Acc | Accs]) -> [[C | Acc] | Accs]
            end,
    Chunks = lists:foldl(Split, [[]], atom_to_list(Atom)),
    [list_to_atom(string:lowercase(Chunk)) || Chunk <- Chunks, Chunk /= []].

human_readable(NormalForm) ->
    lists:reverse([list_to_atom(string:reverse(atom_to_list(Chunk))) || Chunk <- NormalForm]).

normalize_test() ->
    Cases = [
        {'',         []},
        {'-',        []},
        {'_',        []},
        {a,          [a]},
        {'A',        [a]},
        {abc_xyz,    [abc, xyz]},
        {abc__xyz,   [abc, xyz]},
        {'abc-xyz',  [abc, xyz]},
        {'abc--xyz', [abc, xyz]},
        {abcXyz,     [abc, xyz]},
        {abc_Xyz,    [abc, xyz]},
        {'abc-Xyz',  [abc, xyz]},
        {abc__Xyz,   [abc, xyz]},
        {'abc--Xyz', [abc, xyz]},
        {'AbcXyz',   [abc, xyz]},
        {'Abc_Xyz',  [abc, xyz]},
        {'Abc-Xyz',  [abc, xyz]},
        {'Abc__Xyz', [abc, xyz]},
        {'Abc--Xyz', [abc, xyz]},
        {'ABC_XYZ',  [abc, xyz]},
        {'ABC-XYZ',  [abc, xyz]},
        {'ABC__XYZ', [abc, xyz]},
        {'ABC--XYZ', [abc, xyz]}
    ],
    lists:foreach(fun ({Atom, ExpectedNormalForm}) ->
                      ActualNormalForm = normalize(Atom),
                      ?assertEqual(ExpectedNormalForm, human_readable(ActualNormalForm), {Atom, ActualNormalForm})
                  end, Cases).
