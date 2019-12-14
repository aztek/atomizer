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

-spec list_atoms(atoms()) -> ok.
list_atoms(Atoms) ->
    case lists:sort(maps:keys(Atoms)) of
        []   -> io:format("No atoms~n", []);
        Keys -> lists:foreach(fun (Atom) -> io:format("~p~n", [Atom]) end, Keys)
    end.

-spec show_atoms(atoms(), boolean()) -> ok.
show_atoms(Atoms, Verbose) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms), Verbose) end,
                  lists:sort(maps:keys(Atoms))).

-spec show_atom(atom(), locations(), boolean()) -> ok.
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

-spec show_location(file:filename(), non_neg_integer() | [position()]) -> ok.
show_location(Filename, NrPositions) when is_integer(NrPositions) ->
    io:format("~s (~w ~s)~n",
              [Filename, NrPositions, plural(NrPositions, "occurrence", "occurrences")]);

show_location(Filename, Positions) ->
    lists:foreach(fun (Position) -> io:format("~s:~w~n", [Filename, Position]) end,
                  lists:sort(sets:to_list(Positions))).

-spec warn_atoms(atoms(), warnings(), boolean()) -> ok.
warn_atoms(Atoms, Warnings, Verbose) ->
    NrAtoms = maps:size(Atoms),
    NrWarnings = sets:size(Warnings),
    io:format("Found ~p ~s in ~p ~s~n",
              [NrWarnings, plural(NrWarnings, "warning", "warnings"),
               NrAtoms,    plural(NrAtoms,    "atom",     "atoms")]),
    lists:foreach(fun (Warning) -> warn_atom(Atoms, Warning, Verbose) end,
                  lists:sort(sets:to_list(Warnings))).

plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.

-spec warn_atom(atomizer:atoms(), Warning :: {atom(), atom()}, Verbose :: boolean()) -> ok.
warn_atom(Atoms, {A, B}, Verbose) ->
    io:format("Possible typo: ~w vs ~w~n~n", [A, B]),
    show_atom(A, maps:get(A, Atoms), Verbose),
    show_atom(B, maps:get(B, Atoms), Verbose).
