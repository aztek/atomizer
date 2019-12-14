-module(atomizer_cli).

-include("atomizer.hrl").

-export([
    main/1,
    run/3
]).

-spec main([string()]) -> ok.
main(_CmdArgs) ->
    run(warn, {dirs, ["."]}, true).

-spec run(list | show | warn, source(), boolean()) -> ok.
run(Action, Source, Verbose) ->
    AtomsTable = ets:new(atoms, [public, bag]),
    WarningsTable = ets:new(warnings, [public, set]),
    case atomizer:atomize(Source, AtomsTable, WarningsTable) of
        ok ->
            case Action of
                list -> list_atoms(AtomsTable);
                show -> show_atoms(AtomsTable, Verbose);
                warn -> warn_atoms(AtomsTable, WarningsTable, Verbose)
            end;

        {error, Error} ->
            io:format(standard_error, "Error: ~p~n", [Error])
    end,
    ets:delete(WarningsTable),
    ets:delete(AtomsTable),
    ok.

-spec list_atoms(ets:tid()) -> ok.
list_atoms(AtomsTable) ->
    case ets:info(AtomsTable, size) of
        0 -> io:format("No atoms~n", []);
        _ -> lists:foreach(fun list_atom/1, list_keys(AtomsTable))
    end.

list_keys(Tab) -> lists:sort(list_keys_helper(Tab, ets:first(Tab))).
list_keys_helper(_, '$end_of_table') -> [];
list_keys_helper(Tab, Key) -> [Key | list_keys_helper(Tab, ets:next(Tab, Key))].

-spec list_atom(atom()) -> ok.
list_atom(Atom) -> io:format("~p~n", [Atom]).

-spec show_atoms(ets:tid(), boolean()) -> ok.
show_atoms(AtomsTable, Verbose) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, AtomsTable, Verbose) end, list_keys(AtomsTable)).

-spec show_atom(atom(), ets:tid(), boolean()) -> ok.
show_atom(Atom, AtomsTable, Verbose) ->
    io:format("~p~n", [Atom]),
    Locations = lists:foldl(fun({F, P}, M) ->
                                Ps = maps:get(F, M, sets:new()),
                                maps:put(F, sets:add_element(P, Ps), M)
                            end, #{}, [L || {_, L} <- ets:lookup(AtomsTable, Atom)]),
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    lists:foreach(fun ({File, Positions, NrPositions}) ->
                      case Verbose of
                          true  -> show_location(File, Positions);
                          false -> show_location(File, NrPositions)
                      end
                  end,
                  lists:reverse(lists:keysort(3, Info))),
    io:format("~n").

-spec show_location(file:filename(), non_neg_integer() | [position()]) -> ok.
show_location(Filename, NrPositions) when is_integer(NrPositions) ->
    io:format("~s (~w ~s)~n",
              [Filename, NrPositions, plural(NrPositions, "occurrence", "occurrences")]);

show_location(Filename, Positions) ->
    lists:foreach(fun (Position) -> io:format("~s:~w~n", [Filename, Position]) end, Positions).

-spec warn_atoms(ets:tid(), ets:tid(), boolean()) -> ok.
warn_atoms(AtomsTable, WarningsTable, Verbose) ->
    NrAtoms = ets:info(AtomsTable, size),
    NrWarnings = ets:info(WarningsTable, size),
    io:format("Found ~p ~s in ~p ~s~n",
              [NrWarnings, plural(NrWarnings, "warning", "warnings"),
               NrAtoms,    plural(NrAtoms,    "atom",     "atoms")]),
    ets:foldl(fun ({Warning}, _) -> warn_atom(AtomsTable, Warning, Verbose) end, ok, WarningsTable).

plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.

-spec warn_atom(ets:tid(), {atom(), atom()}, boolean()) -> ok.
warn_atom(AtomsTable, {A, B}, Verbose) ->
    io:format("Possible typo: ~w vs ~w~n~n", [A, B]),
    show_atom(A, AtomsTable, Verbose),
    show_atom(B, AtomsTable, Verbose).
