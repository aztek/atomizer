-module(atomizer_normalize).

-include_lib("eunit/include/eunit.hrl").

-export([normalize/1]).

-include("atomizer.hrl").

-define(IS_ALPHA_LOWER(C), C >= $a, C =< $z).
-define(IS_ALPHA_UPPER(C), C >= $A, C =< $Z).
-define(IS_DIGIT(C),       C >= $0, C =< $9).
-define(IS_DELIMITER(C),   C == $_; C == $-).

-spec normalize(atom()) -> [normal_form()].
%% Normalize atom names spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination.
normalize(Atom) ->
    Split = fun (C, Accs) when ?IS_DELIMITER(C) -> [[] | Accs];
                (C, [[C0 | Acc] | Accs]) when ?IS_ALPHA_UPPER(C), ?IS_ALPHA_UPPER(C0) -> [[C, C0 | Acc] | Accs];
                (C, Accs) when ?IS_ALPHA_UPPER(C) -> [[C] | Accs];
                (C, [Acc | Accs]) -> [[C | Acc] | Accs]
            end,
    Chunks = lists:foldl(Split, [[]], atom_to_list(Atom)),
    ChonkyBoi = [string:lowercase(Chunk) || Chunk <- Chunks, Chunk /= []],
    Concatenations = concatenations(ChonkyBoi),
    [lists:map(fun list_to_atom/1, NormalForm) || NormalForm <- Concatenations].

-spec concatenations([List]) -> [nonempty_list(List)] when List :: nonempty_list().
%% Generate all combinations of concatenations of adjacent list elements.
%%
%%   concatenations([]) == []
%%   concatenations([C]) == [[C]]
%%   concatenations([B, C]) == [[B, C], [B ++ C]]
%%   concatenations([A, B, C]) == [[A, B, C], [A, B ++ C], [A ++ B, C], [A ++ B ++ C]]
%%
concatenations([]) -> [];
concatenations([E]) -> [[E]];
concatenations([E | Es]) ->
    Cs = concatenations(Es),
    [[E | C] || C <- Cs] ++ [[E ++ H | T] || [H | T] <- Cs].

concatenation_test() ->
    Cases = [
        {[],                   []},
        {["c"],                [["c"]]},
        {["b", "c"],           [["b", "c"], ["bc"]]},
        {["a", "b", "c"],      [["a", "b", "c"], ["a", "bc"], ["ab", "c"], ["abc"]]},
        {["a", "b", "c", "d"], [["a","b","c","d"],
                                ["a","b","cd"],
                                ["a","bc","d"],
                                ["a","bcd"],
                                ["ab","c","d"],
                                ["ab","cd"],
                                ["abc","d"],
                                ["abcd"]]}
    ],
    lists:foreach(fun ({List, Concatenations}) ->
                      ?assertEqual(Concatenations, concatenations(List))
                  end, Cases).

human_readable(NormalForms) ->
    [lists:reverse([list_to_atom(string:reverse(atom_to_list(Chunk))) || Chunk <- NormalForm]) || NormalForm <- NormalForms].

normalize_test() ->
    Cases = [
        {'',         []},
        {'-',        []},
        {'_',        []},
        {a,          [[a]]},
        {'A',        [[a]]},
        {abc_xyz,    [[abc, xyz], [abcxyz]]},
        {abc__xyz,   [[abc, xyz], [abcxyz]]},
        {'abc-xyz',  [[abc, xyz], [abcxyz]]},
        {'abc--xyz', [[abc, xyz], [abcxyz]]},
        {abcXyz,     [[abc, xyz], [abcxyz]]},
        {abc_Xyz,    [[abc, xyz], [abcxyz]]},
        {'abc-Xyz',  [[abc, xyz], [abcxyz]]},
        {abc__Xyz,   [[abc, xyz], [abcxyz]]},
        {'abc--Xyz', [[abc, xyz], [abcxyz]]},
        {'AbcXyz',   [[abc, xyz], [abcxyz]]},
        {'Abc_Xyz',  [[abc, xyz], [abcxyz]]},
        {'Abc-Xyz',  [[abc, xyz], [abcxyz]]},
        {'Abc__Xyz', [[abc, xyz], [abcxyz]]},
        {'Abc--Xyz', [[abc, xyz], [abcxyz]]},
        {'ABC_XYZ',  [[abc, xyz], [abcxyz]]},
        {'ABC-XYZ',  [[abc, xyz], [abcxyz]]},
        {'ABC__XYZ', [[abc, xyz], [abcxyz]]},
        {'ABC--XYZ', [[abc, xyz], [abcxyz]]}
    ],
    lists:foreach(fun ({Atom, ExpectedNormalForm}) ->
                      ActualNormalForm = normalize(Atom),
                      ?assertEqual(ExpectedNormalForm, human_readable(ActualNormalForm), {Atom, ActualNormalForm})
                  end, Cases).
