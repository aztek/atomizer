-module(atomizer_compare).

-include("atomizer.hrl").

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
                                          [{_, Btom}] ->
                                              Pid ! {warning, Atom, Btom, nf_mismatch};
                                          [] ->
                                              ets:insert(NFs, {NormalForm, Atom})
                                      end
                                  end,
                                  atomizer_normalize:normalize(Atom))
            end,
            loop(Pid, Atoms, NFs);

        done_atoms ->
            Pid ! done_warnings,
            ok
    end.

insignificant(Atom) ->
    length(atom_to_list(Atom)) =< 2.
