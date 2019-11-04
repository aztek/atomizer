-module(atomizer_cli).

-export([
    main/1,
    list_files/1,
    show_files/1,
    warn_files/1,
    show_dirs/1,
    list_dirs/1,
    warn_dirs/1
]).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    show(CmdArgs).

list_files(Files) -> list({files, Files}).
list_dirs(Dirs)   -> list({dirs, Dirs}).

list(Source) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, _} -> list_atoms(Atoms);
        {error, Error} -> io:format(standard_error, "Error: ~p~n", [Error])
    end.

-spec list_atoms(multimaps:multimap(atom(), atomizer:location())) -> ok.
list_atoms(Atoms) ->
    case lists:sort(maps:keys(Atoms)) of
        []   -> io:format("No atoms~n", []);
        Keys -> lists:foreach(fun (Atom) -> io:format("~p~n", [Atom]) end, Keys)
    end.

show_files(Files) -> show({files, Files}).
show_dirs(Dirs)   -> show({dirs, Dirs}).

show(Source) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, _} -> show_atoms(Atoms);
        {error, Error} -> io:format(standard_error, "Error: ~p~n", [Error])
    end.

-spec show_atoms(multimaps:multimap(atom(), atomizer:location())) -> ok.
show_atoms(Atoms) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms)) end,
                  lists:sort(maps:keys(Atoms))).

-spec show_atom(atom(), sets:set(atomizer:location())) -> ok.
show_atom(Atom, Locations) ->
    io:format("~p~n", [Atom]),
    lists:foreach(fun show_location/1, lists:sort(sets:to_list(Locations))),
    io:format("~n").

-spec show_location(atomizer:location()) -> ok.
show_location({F, L}) -> io:format("~s:~p~n", [filename:absname(F), L]).

warn_files(Files) -> warn({files, Files}).
warn_dirs(Dirs)   -> warn({dirs, Dirs}).

warn(Source) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, Warnings} ->
            NrWarnings = length(sets:to_list(Warnings)),
            NrAtoms = length(maps:keys(Atoms)),
            io:format("Found ~p warning(s) in ~p atoms(s)~n", [NrWarnings, NrAtoms]),
            lists:foreach(fun (Warning) -> warn(Atoms, Warning) end,
                          sets:to_list(Warnings));

        {error, Error} ->
            io:format(standard_error, "Error: ~p~n", [Error])
    end.

warn(Atoms, {A, B}) ->
    io:format("Possible typo: ~w vs ~w~n~n", [A, B]),
    show_atom(A, maps:get(A, Atoms)),
    show_atom(B, maps:get(B, Atoms)).
