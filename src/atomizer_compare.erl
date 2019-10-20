-module(atomizer_compare).

-export([compare/1]).

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
                    Pid = self(),
                    lists:foreach(fun (Btom) ->
                                      spawn_link(fun () ->
                                                     Pid ! {comparison, Atom, Btom, possible_typo(Atom, Btom)}
                                                 end)
                                  end,
                                  sets:to_list(Atoms)),
                    loop(sets:add_element(Atom, Atoms), InProgress + sets:size(Atoms), DoneAtoms)
            end;

        {comparison, Atom, Btom, {yes, Reason}} ->
            get(callback) ! {warn, Atom, Btom, Reason},
            loop(Atoms, InProgress - 1, DoneAtoms);

        {comparison, _, _, no} ->
            loop(Atoms, InProgress - 1, DoneAtoms);

        done_atoms ->
            loop(Atoms, InProgress, true)
    end.

-spec possible_typo(S :: atom(), T :: atom()) -> {yes, Info :: term()} | no.
possible_typo(S, T) ->
    D = levenshtein(atom_to_list(S), atom_to_list(T)),
    case D of
        _ when D < 2 -> {yes, {levenshtein, D}};
        _ -> no
    end.

-spec levenshtein(S :: string(), T :: string()) -> Distance :: integer().
levenshtein(S, T) ->
    {D, _} = levenshtein(S, T, #{}),
    D.

-spec levenshtein(S :: string(), T :: string(), Cache) -> Distance :: integer() when
    Cache :: #{{S :: string(), T :: string()} => Distance :: integer()}.
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
