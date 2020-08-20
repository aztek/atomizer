-module(atomizer_compare).

-export([
    start/0,
    atom/1,
    stop/0
]).

-define(PROCESS_NAME, ?MODULE).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn_link(fun () -> compare() end)).

-spec atom(atom()) -> ok.
atom(Atom) ->
    ?PROCESS_NAME ! {atom, Atom},
    ok.

-spec stop() -> ok.
stop() ->
    ?PROCESS_NAME ! done_atoms,
    ok.

-spec compare() -> ok.
compare() ->
    Atoms = ets:new(atoms, [private, set]),
    NFs = ets:new(nfs, [private, bag]),
    loop(Atoms, NFs),
    ets:delete(NFs),
    ets:delete(Atoms),
    ok.

-spec loop(ets:tid(), ets:tid()) -> ok.
loop(Atoms, NFs) ->
    receive
        {atom, Atom} ->
            case insignificant(Atom) orelse ets:member(Atoms, Atom) of
                true -> ignore;
                false ->
                    ets:insert(Atoms, {Atom}),
                    lists:foreach(fun (NormalForm) ->
                                      case ets:lookup(NFs, NormalForm) of
                                          [{_, Lookalike}] ->
                                              atomizer_sup:lookalikes(Atom, Lookalike);
                                          [] ->
                                              ets:insert(NFs, {NormalForm, Atom})
                                      end
                                  end,
                                  atomizer_normalize:normalize(Atom))
            end,
            loop(Atoms, NFs);

        done_atoms ->
            atomizer_sup:stop()
    end.

insignificant(Atom) ->
    length(atom_to_list(Atom)) =< 2.
