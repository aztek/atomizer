-module(atomizer_compare).

-include("atomizer.hrl").

-export([compare/1]).

-spec compare(pid()) -> ok.
compare(Pid) ->
    ets:new(atoms, [private, named_table, set]),
    Result = loop(Pid, 0, false),
    ets:delete(atoms),
    Result.

-spec loop(pid(), non_neg_integer(), boolean()) -> ok.
loop(Pid, 0, true) ->
    Pid ! done_warnings,
    ok;

loop(Pid, InProgress, DoneAtoms) ->
    receive
        {atom, Atom} ->
            case ets:member(atoms, Atom) of
                true ->
                    loop(Pid, InProgress, DoneAtoms);

                false ->
                    NrComparisons = compare_all(Atom),
                    ets:insert(atoms, {Atom}),
                    loop(Pid, InProgress + NrComparisons, DoneAtoms)
            end;

        {comparison, Atom, Btom, Result} ->
            case Result of
                {yes, Reason} -> Pid ! {warning, Atom, Btom, Reason};
                no -> ok
            end,
            loop(Pid, InProgress - 1, DoneAtoms);

        done_atoms ->
            loop(Pid, InProgress, true)
    end.

-spec compare_all(atom()) -> non_neg_integer().
compare_all(Atom) ->
    Pid = self(),
    ets:foldl(fun ({Btom}, NrComparisons) ->
        spawn_link(fun() -> Pid ! {comparison, Atom, Btom, possible_typo(Atom, Btom)} end),
        NrComparisons + 1
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
