-module(atomizer_cli).

-export([
    main/1,
    run/3
]).

-spec main([string()]) -> ok.
main(_CmdArgs) ->
    run(warn, {dirs, ["."]}, true).

-spec run(Action, Source, Verbose) -> ok when
    Action :: list | show | warn,
    Source :: atomizer:source(),
    Verbose :: boolean().
run(Action, Source, Verbose) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, Warnings} ->
            case Action of
                list -> list_atoms(Atoms);
                show -> show_atoms(Atoms, Verbose);
                warn -> warn_atoms(Atoms, Warnings, Verbose)
            end;

        {error, Error} ->
            io:format(standard_error, "Error: ~p~n", [Error])
    end.

-spec list_atoms(atomizer:atoms()) -> ok.
list_atoms(Atoms) ->
    case lists:sort(maps:keys(Atoms)) of
        []   -> io:format("No atoms~n", []);
        Keys -> lists:foreach(fun (Atom) -> io:format("~p~n", [Atom]) end, Keys)
    end.

-spec show_atoms(atomizer:atoms(), boolean()) -> ok.
show_atoms(Atoms, Verbose) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms), Verbose) end,
                  lists:sort(maps:keys(Atoms))).

-spec show_atom(atom(), atomizer:locations(), boolean()) -> ok.
show_atom(Atom, Locations, Verbose) ->
    io:format("~p~n", [Atom]),
    Info = [{filename:absname(File), Positions, sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    lists:foreach(fun ({File, Positions, NrPositions}) ->
                      case Verbose of
                          true  -> show_location(File, Positions);
                          false -> show_location(File, NrPositions)
                      end
                  end,
                  lists:keysort(3, Info)),
    io:format("~n").

-spec show_location(file:filename(), non_neg_integer() | atomizer:positions()) -> ok.
show_location(Filename, NrPositions) when is_integer(NrPositions) ->
    io:format("~s (~w ~s)~n",
              [Filename, NrPositions, plural(NrPositions, "occurrence", "occurrences")]);

show_location(Filename, Positions) ->
    lists:foreach(fun (Position) -> io:format("~s:~w~n", [Filename, Position]) end,
                  lists:sort(sets:to_list(Positions))).

-spec warn_atoms(atomizer:atoms(), Warnings :: [{atom(), atom()}], Verbose :: boolean()) -> ok.
warn_atoms(Atoms, Warnings, Verbose) ->
    NrAtoms = length(maps:keys(Atoms)),
    NrWarnings = length(Warnings),
    io:format("Found ~p ~s in ~p ~s~n",
              [NrWarnings, plural(NrWarnings, "warning", "warnings"),
               NrAtoms,    plural(NrAtoms,    "atom",     "atoms")]),
    lists:foreach(fun (Warning) -> warn_atom(Atoms, Warning, Verbose) end, Warnings).

plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.

-spec warn_atom(atomizer:atoms(), Warning :: {atom(), atom()}, Verbose :: boolean()) -> ok.
warn_atom(Atoms, {A, B}, Verbose) ->
    io:format("Possible typo: ~w vs ~w~n~n", [A, B]),
    show_atom(A, maps:get(A, Atoms), Verbose),
    show_atom(B, maps:get(B, Atoms), Verbose).
