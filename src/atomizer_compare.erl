-module(atomizer_compare).

-include("atomizer.hrl").

-export([compare/1]).

-spec compare(pid()) -> ok.
compare(Pid) ->
    ets:new(atoms, [private, named_table, set]),
    ets:new(nfs, [private, named_table, bag]),
    Result = loop(Pid, false),
    ets:delete(nfs),
    ets:delete(atoms),
    Result.

-spec loop(pid(), boolean()) -> ok.
loop(Pid, true) ->
    Pid ! done_warnings,
    ok;

loop(Pid, DoneAtoms) ->
    receive
        {atom, Atom} ->
            case insignificant(Atom) of
                true -> ignore;
                false ->
                    case ets:member(atoms, Atom) of
                        true -> ignore;
                        false ->
                            ets:insert(atoms, {Atom}),
                            case atomizer_normalize:normalize(Atom) of
                                [] -> ignore;
                                NormalForm ->
                                    case ets:lookup(nfs, NormalForm) of
                                        [{_, Btom}] ->
                                            Pid ! {warning, Atom, Btom, nf_mismatch};
                                        [] ->
                                            ets:insert(nfs, {NormalForm, Atom})
                                    end
                            end
                    end
            end,
            loop(Pid, DoneAtoms);

        done_atoms ->
            loop(Pid, true)
    end.

insignificant(Atom) ->
    length(atom_to_list(Atom)) == 1.
