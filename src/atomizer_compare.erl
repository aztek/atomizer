-module(atomizer_compare).

-export([compare/1, levenshtein/2]).

compare(Callback) ->
    put(callback, Callback),
    loop(sets:new(), 0, false).

loop(_, 0, true) ->
    get(callback) ! done_warnings;

loop(Atoms, InProgress, DoneAtoms) ->
    receive
        {atom, Atom} ->
            case sets:is_element(Atom, Atoms) of
                true ->
                    loop(Atoms, InProgress, DoneAtoms);

                false ->
                    compare_all(Atom, Atoms),
                    loop(sets:add_element(Atom, Atoms), InProgress + sets:size(Atoms), DoneAtoms)
            end;

        {comparison, Atom, Btom, {yes, Reason}} ->
            get(callback) ! {warning, Atom, Btom, Reason},
            loop(Atoms, InProgress - 1, DoneAtoms);

        {comparison, _, _, no} ->
            loop(Atoms, InProgress - 1, DoneAtoms);

        done_atoms ->
            loop(Atoms, InProgress, true)
    end.

compare_all(Atom, Atoms) ->
    lists:foreach(fun (Btom) -> compare(Atom, Btom) end, sets:to_list(Atoms)).

compare(Atom, Btom) ->
    Pid = self(),
    spawn_link(fun () -> Pid ! {comparison, Atom, Btom, possible_typo(Atom, Btom)} end).

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
