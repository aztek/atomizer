-module(atomizer_cli).

-export([
    main/1,
    list_files/1,
    show_files/1,
    show_files/2,
    warn_files/1,
    warn_files/2,
    list_dirs/1,
    show_dirs/1,
    show_dirs/2,
    warn_dirs/1,
    warn_dirs/2
]).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    show(CmdArgs, true).

list_files(Files) -> list({files, Files}).
list_dirs(Dirs)   -> list({dirs, Dirs}).

list(Source) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, _} -> list_atoms(Atoms);
        {error, Error} -> io:format(standard_error, "Error: ~p~n", [Error])
    end.

-spec list_atoms(atomizer:atoms()) -> ok.
list_atoms(Atoms) ->
    case lists:sort(maps:keys(Atoms)) of
        []   -> io:format("No atoms~n", []);
        Keys -> lists:foreach(fun (Atom) -> io:format("~p~n", [Atom]) end, Keys)
    end.

show_files(Files) -> show_files(Files, false).
show_files(Files, Verbose) -> show({files, Files}, Verbose).

show_dirs(Dirs) -> show_dirs(Dirs, false).
show_dirs(Dirs, Verbose)  -> show({dirs, Dirs}, Verbose).

show(Source, Verbose) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, _} -> show_atoms(Atoms, Verbose);
        {error, Error} -> io:format(standard_error, "Error: ~p~n", [Error])
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
    Occurrences = case NrPositions of
                      1 -> "occurrence";
                      _ -> "occurrences"
                  end,
    io:format("~s (~w ~s)~n", [Filename, NrPositions, Occurrences]);

show_location(Filename, Positions) ->
    lists:foreach(fun (Position) -> io:format("~s:~w~n", [Filename, Position]) end,
                  lists:sort(sets:to_list(Positions))).

warn_files(Files) -> warn_files(Files, false).
warn_files(Files, Verbose) -> warn({files, Files}, Verbose).

warn_dirs(Dirs) -> warn_dirs(Dirs, false).
warn_dirs(Dirs, Verbose) -> warn({dirs, Dirs}, Verbose).

warn(Source, Verbose) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, Warnings} ->
            NrWarnings = length(sets:to_list(Warnings)),
            NrAtoms = length(maps:keys(Atoms)),
            io:format("Found ~p warning(s) in ~p atoms(s)~n", [NrWarnings, NrAtoms]),
            lists:foreach(fun (Warning) -> warn(Atoms, Warning, Verbose) end,
                          sets:to_list(Warnings));

        {error, Error} ->
            io:format(standard_error, "Error: ~p~n", [Error])
    end.

warn(Atoms, {A, B}, Verbose) ->
    io:format("Possible typo: ~w vs ~w~n~n", [A, B]),
    show_atom(A, maps:get(A, Atoms), Verbose),
    show_atom(B, maps:get(B, Atoms), Verbose).
