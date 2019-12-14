-module(atomizer_compare).

-include("atomizer.hrl").

-export([compare/1, levenshtein/2]).

-spec compare(pid()) -> ok.
compare(Callback) ->
    put(callback, Callback),
    ets:new(atoms, [private, named_table, set]),
    loop(0, false),
    ets:delete(atoms).

-spec loop(non_neg_integer(), boolean()) -> ok.
loop(0, true) ->
    get(callback) ! done_warnings;

loop(InProgress, DoneAtoms) ->
    receive
        {atom, Atom} ->
            case ets:member(atoms, Atom) of
                true ->
                    loop(InProgress, DoneAtoms);

                false ->
                    NrComparisons = compare_all(Atom),
                    ets:insert(atoms, {Atom}),
                    loop(InProgress + NrComparisons, DoneAtoms)
            end;

        {comparison, Atom, Btom, Result} ->
            case Result of
                {yes, Reason} -> get(callback) ! {warning, Atom, Btom, Reason};
                no -> ok
            end,
            loop(InProgress - 1, DoneAtoms);

        done_atoms ->
            loop(InProgress, true)
    end.

-spec compare_all(atom()) -> ok.
compare_all(Atom) ->
    Pid = self(),
    ets:foldl(fun ({Btom}, Comparisons) ->
                  spawn_link(fun() -> Pid ! {comparison, Atom, Btom, possible_typo(Atom, Btom)} end),
                  Comparisons + 1
              end, 0, atoms).

-spec possible_typo(A :: atom(), B :: atom()) -> {yes, Info :: term()} | no.
possible_typo(A, B) ->
    S = atom_to_list(A),
    T = atom_to_list(B),
    D = similar(S, T),
    if
        length(S) > 1, length(T) > 1, D -> {yes, similar};
        true -> no
    end.

-spec similar(A :: string(), B :: string()) -> boolean().
%% Check whether two strings are different in exactly one character.
%% In other words, that their Levenshtein distance is 1.
similar([C | As], [C | Bs]) -> similar(As, Bs);
similar([_ | As], [_ | Bs]) -> As == Bs;
similar([], [_]) -> true;
similar([_], []) -> true;
similar(_, _) -> false.

-spec levenshtein(S :: string(), T :: string()) -> Distance :: integer().
levenshtein(S, T) ->
    {D, _} = levenshtein(S, T, #{}),
    D.

-spec levenshtein(string(), string(), Cache) -> {Distance, Cache} when
    Distance :: non_neg_integer(),
    Cache    :: #{{string(), string()} => Distance}.
levenshtein([], T, C) -> {length(T), C#{{[], T} => length(T)}};
levenshtein(S, [], C) -> {length(S), C#{{S, []} => length(S)}};
levenshtein([X | S], [X | T], C) -> levenshtein(S, T, C);
levenshtein([_ | ST] = S, [_ | TT] = T, C) ->
    case maps:find({S, T}, C) of
        {ok, D} -> {D, C};
        error   -> {L1, C1} = levenshtein(S, TT, C),
                   {L2, C2} = levenshtein(ST, T, C1),
                   {L3, C3} = levenshtein(ST, TT, C2),
                   L = 1 + lists:min([L1, L2, L3]),
                   {L, C3#{{S, T} => L}}
    end.
