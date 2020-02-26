-module(atomizer_compare).

-export([compare/1]).

-spec compare(pid()) -> ok.
compare(Pid) ->
    Atoms = ets:new(atoms, [private, set]),
    NFs = ets:new(nfs, [private, bag]),
    loop(Pid, Atoms, NFs),
    ets:delete(NFs),
    ets:delete(Atoms),
    ok.

-spec loop(pid(), ets:tid(), ets:tid()) -> ok.
loop(Pid, Atoms, NFs) ->
    receive
        {atom, Atom} ->
            case insignificant(Atom) orelse ets:member(Atoms, Atom) of
                true -> ignore;
                false ->
                    ets:insert(Atoms, {Atom}),
                    lists:foreach(fun (NormalForm) ->
                                      case ets:lookup(NFs, NormalForm) of
                                          [{_, Lookalike}] ->
                                              Pid ! {lookalikes, Atom, Lookalike};
                                          [] ->
                                              ets:insert(NFs, {NormalForm, Atom})
                                      end
                                  end,
                                  atomizer_normalize:normalize(Atom))
            end,
            loop(Pid, Atoms, NFs);

        done_atoms ->
            Pid ! done_comparing,
            ok
    end.

insignificant(Atom) ->
    length(atom_to_list(Atom)) =< 2.
