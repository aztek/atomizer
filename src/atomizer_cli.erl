-module(atomizer_cli).

-include("atomizer.hrl").

-type action() :: list | show | warn.
-type verbosity() :: 0 | 1 | 2. % 0 is least verbose, 2 is most verbose

-define(DEFAULT_ACTION, warn).
-define(DEFAULT_SOURCE, {dir, "."}).
-define(DEFAULT_VERBOSITY, 1).

-export([
    main/1,
    run/1,
    run/2,
    run/3
]).

-spec main([string()]) -> ok.
main(_CmdArgs) ->
    run(?DEFAULT_SOURCE).

-spec run(source()) -> ok.
run(Source) -> run(?DEFAULT_ACTION, Source, ?DEFAULT_VERBOSITY).

-spec run(action(), source()) -> ok.
run(Action, Source) -> run(Action, Source, ?DEFAULT_VERBOSITY).

-spec run(action(), source(), verbosity()) -> ok.
run(Action, Source, Verbosity) ->
    case atomizer:atomize(Source) of
        {ok, Atoms, Warnings, NrParsed} ->
            case Action of
                list -> list_atoms(Atoms);
                show -> show_atoms(Atoms, Verbosity);
                warn -> warn_atoms(Atoms, Warnings, NrParsed, Verbosity)
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

-spec show_atoms(atoms(), verbosity()) -> ok.
show_atoms(Atoms, Verbosity) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms), Verbosity) end,
                  lists:sort(maps:keys(Atoms))).

show_abridged_list(Printer, List, Verbosity) ->
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            io:format("\e[3m... (~p more)\e[00m~n", [ListLength - PreviewLength]);
        _ ->
            lists:foreach(Printer, List)
    end.

-spec show_atom(atom(), locations(), verbosity()) -> ok.
show_atom(Atom, Locations, Verbosity) ->
    io:format("~n\e[1m~p\e[00m~n", [Atom]),
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    Files = lists:reverse(lists:keysort(3, Info)),
    ShowLocation = fun ({File, Positions, NrPositions}) -> show_location(File, Positions, NrPositions, Verbosity) end,
    show_abridged_list(ShowLocation, Files, Verbosity).

-spec show_location(file:filename(), [position()], non_neg_integer(), verbosity()) -> ok.
show_location(File, Positions, _, Verbosity) when Verbosity >= 1 ->
    ShowPosition = fun (Position) -> io:format("~s:~w~n", [File, Position]) end,
    show_abridged_list(ShowPosition, Positions, Verbosity);

show_location(File, _, NrPositions, _) ->
    io:format("~s \e[3m(~w ~s)\e[00m~n",
              [File, NrPositions, plural(NrPositions, "occurrence", "occurrences")]).

-spec warn_atoms(atoms(), warnings(), non_neg_integer(), verbosity()) -> ok.
warn_atoms(Atoms, Warnings, NrParsed, Verbosity) ->
    NrAtoms = maps:size(Atoms),
    NrWarnings = sets:size(Warnings),
    lists:foreach(fun (Warning) -> warn_atom(Atoms, Warning, Verbosity) end,
                  lists:sort(sets:to_list(Warnings))),
    io:format("Found \e[1m~p\e[00m ~s of similar atoms among "
              "the total of \e[1m~p\e[00m ~s in \e[1m~p\e[00m ~s.~n",
              [NrWarnings, plural(NrWarnings, "pair", "pairs"),
               NrAtoms,    plural(NrAtoms,    "atom", "atoms"),
               NrParsed,   plural(NrParsed,   "file", "files")]).
 
plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.

-spec warn_atom(atoms(), warning(), verbosity()) -> ok.
warn_atom(Atoms, {A, B}, Verbosity) ->
    io:format("\e[36m\e[1m~p\e[00m\e[36m vs \e[1m~p\e[00m~n", [A, B]),
    show_atom(A, maps:get(A, Atoms), Verbosity),
    show_atom(B, maps:get(B, Atoms), Verbosity),
    io:format("~n~n", []).
