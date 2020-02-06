-module(atomizer_cli).

-export([
    main/1,
    run/1,
    run/2,
    run/3
]).

-include("atomizer.hrl").

-type action() :: list | show | warn.
-type verbosity() :: 0 | 1 | 2. % 0 is least verbose, 2 is most verbose

-record(options, {
    action     = warn  :: action(),
    paths      = []    :: [file:filename()],
    includes   = []    :: [file:filename()],
    parse_beam = false :: boolean(),
    verbosity  = 1     :: verbosity()
}).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    case parse_args(CmdArgs) of
        {options, Options} ->
            case run(Options) of
                ok ->
                    halt(0);

                {error, {Module, Error}} ->
                    ?PRINT_ERROR(Module:format_error(Error)),
                    halt(1)
            end;

        {message, Message} ->
            io:format("~s~n", [Message]),
            halt(0);

        {error, Error} ->
            ?PRINT_ERROR(Error),
            halt(1)
    end.

-spec parse_args([string()]) -> {options, #options{}} | {message, string()} | {error, string()}.
parse_args(CmdArgs) -> parse_args(CmdArgs, #options{}).

-spec parse_args([string()], #options{}) -> {options, #options{}} | {message, string()} | {error, string()}.
parse_args([], Options) -> {options, Options};
parse_args([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch ->
            parse_args(CmdArgs, Options#options{paths = [CmdArg | Options#options.paths]});

        Option ->
            parse_option(Option, CmdArgs, Options)
    end.

-spec parse_includes([string()], #options{}) -> {ok, #options{}} | {message, string()} | {error, string()}.
parse_includes([], Options) -> {options, Options};
parse_includes([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch ->
            parse_includes(CmdArgs, Options#options{includes = [CmdArg | Options#options.includes]});

        Option ->
            parse_option(Option, CmdArgs, Options)
    end.

-spec parse_option(string(), [string()], #options{}) -> {ok, #options{}} | {message, string()} | {error, string()}.
parse_option(Option, CmdArgs, Options) ->
    case Option of
        _Help when Option == "h"; Option == "-help" ->
            {message, usage() ++ "\n" ++ help()};

        _Action when Option == "a"; Option == "-action" ->
            case parse_action(CmdArgs) of
                {ok, Action, TailCmdArgs} ->
                    parse_args(TailCmdArgs, Options#options{action = Action});

                {error, Error} ->
                    {error, Error}
            end;

        _Includes when Option == "i"; Option == "I"; Option == "-include" ->
            parse_includes(CmdArgs, Options);

        _ParseBeam when Option == "b"; Option == "-parse-beam" ->
            parse_args(CmdArgs, Options#options{parse_beam = true});

        _Verbosity when Option == "v"; Option == "-verbosity" ->
            case parse_verbosity(Option) of
                {ok, Verbosity, TailCmdArgs} ->
                    parse_args(TailCmdArgs, Options#options{verbosity = Verbosity});

                {error, Error} ->
                    {error, Error}
            end;

        _ ->
            {error, "Unrecognized option."}
    end.

-spec parse_action([string()]) -> {ok, action(), [string()]} | {error, string()}.
parse_action([]) -> {error, "Malformed arguments - action is missing."};
parse_action([CmdArg | CmdArgs]) ->
    case CmdArg of
        "list" -> {ok, list, CmdArgs};
        "show" -> {ok, show, CmdArgs};
        "warn" -> {ok, warn, CmdArgs};
        _ -> {error, "Wrong action. Supported values are list, show and warn."}
    end.

-spec parse_verbosity([string()]) -> {ok, verbosity(), [string()]} | {error, string()}.
parse_verbosity([]) -> {error, "Malformed arguments - verbosity is missing."};
parse_verbosity([CmdArg | CmdArgs]) ->
    case CmdArg of
        "0" -> {ok, 0, CmdArgs};
        "1" -> {ok, 1, CmdArgs};
        "2" -> {ok, 2, CmdArgs};
        _ -> {error, "Wrong verbosity. Supported values are 0, 1 and 2."}
    end.

usage() ->
    "Usage: atomizer [-a | --action ACTION] [-b | --parse-beam] [-v | --verbosity VERBOSITY]\n" ++
    "                PATH* [-i | --include PATH*]\n".

help() ->
    "".

-spec run(#options{} | file:filename()) -> ok | {error, term()}.
run(#options{action = Action, paths = Paths, includes = IncludePaths, parse_beam = ParseBeams, verbosity = Verbosity}) ->
    case atomizer:atomize(Paths, IncludePaths, ParseBeams) of
        {ok, Atoms, Warnings, NrFiles, NrDirs} ->
            SignificantWarnings = sets:filter(fun (Warning) -> is_significant(Atoms, Warning) end, Warnings),
            case Action of
                list -> list_atoms(Atoms, Verbosity);
                show -> show_atoms(Atoms, Verbosity);
                warn -> warn_atoms(Atoms, SignificantWarnings, NrFiles, NrDirs, Verbosity)
            end,
            ok;

        {error, Error} ->
            {error, Error}
    end;

run(Path) ->
    run(#options{paths = [Path]}).

-spec run(action(), [file:filename()]) -> ok.
run(Action, Paths) ->
    run(#options{action = Action, paths = Paths}).

-spec run(action(), [file:filename()], verbosity()) -> ok.
run(Action, Paths, Verbosity) ->
    run(#options{action = Action, paths = Paths, verbosity = Verbosity}).

-spec list_atoms(atoms(), verbosity()) -> ok.
list_atoms(Atoms, Verbosity) ->
    case lists:sort(maps:keys(Atoms)) of
        [] when Verbosity > 1 -> io:format("No atoms found.~n", []);
        [] -> ok;
        Keys -> lists:foreach(fun (Atom) -> list_atom(Atom, maps:get(Atom, Atoms), Verbosity) end, Keys)
    end.

-spec list_atom(atom(), locations(), verbosity()) -> ok.
list_atom(Atom, Locations, Verbosity) ->
    NrOccurrences = nr_occurrences(Locations),
    if
        Verbosity > 1 ->
            NrFiles = nr_files(Locations),
            io:format("~p\t~p\t~p~n", [Atom, NrOccurrences, NrFiles]);

        true ->
            io:format("~p\t~p~n", [Atom, NrOccurrences])
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

-spec warn_atoms(atoms(), warnings(), non_neg_integer(), non_neg_integer(), verbosity()) -> ok.
warn_atoms(Atoms, Warnings, NrFiles, NrDirs, Verbosity) ->
    NrAtoms = maps:size(Atoms),
    NrWarnings = sets:size(Warnings),
    lists:foreach(fun (Warning) -> warn_atom(Atoms, Warning, Verbosity) end,
                  lists:sort(sets:to_list(Warnings))),
    io:format("Found \e[1m~p\e[00m ~s of similar atoms among "
              "\e[1m~p\e[00m ~s in \e[1m~p\e[00m ~s and \e[1m~p\e[00m ~s.~n",
              [NrWarnings, plural(NrWarnings, "pair", "pairs"),
               NrAtoms,    plural(NrAtoms,    "atom", "atoms"),
               NrFiles,    plural(NrFiles,    "file", "files"),
               NrDirs,     plural(NrDirs,     "directory", "directories")]).
 
plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.

-spec warn_atom(atoms(), warning(), verbosity()) -> ok.
warn_atom(Atoms, {A, B}, Verbosity) ->
    io:format("\e[36m\e[1m~p\e[00m\e[36m vs \e[1m~p\e[00m~n", [A, B]),
    show_atom(A, maps:get(A, Atoms), Verbosity),
    show_atom(B, maps:get(B, Atoms), Verbosity),
    io:format("~n~n", []).

-spec is_significant(atoms(), warning()) -> boolean().
is_significant(Atoms, {A, B}) ->
    LocationsA = maps:get(A, Atoms),
    LocationsB = maps:get(B, Atoms),
    NrOccurrenceA = nr_occurrences(LocationsA),
    NrOccurrenceB = nr_occurrences(LocationsB),
    {Typo, Min, Max} =
        if
             NrOccurrenceA < NrOccurrenceB -> {A, NrOccurrenceA, NrOccurrenceB};
             true -> {B, NrOccurrenceB, NrOccurrenceA}
        end,
    Disproportion = 4,
    Disproportional = Max / Min > Disproportion,
    Local = nr_files(maps:get(Typo, Atoms)) == 1,
    Related = not sets:is_disjoint(sets:from_list(maps:keys(LocationsA)), sets:from_list(maps:keys(LocationsB))),
    Disproportional andalso Local andalso Related.

-spec nr_files(locations()) -> non_neg_integer().
nr_files(Locations) -> maps:size(Locations).

-spec nr_occurrences(locations()) -> non_neg_integer().
nr_occurrences(Locations) ->
    maps:fold(fun (_, V, S) -> sets:size(V) + S end, 0, Locations).
