-module(atomizer_cli).

-export([
    main/1,
    run/1,
    run/2,
    run/3
]).

-type action() :: list | show | warn.
-type verbosity() :: 0 | 1 | 2. % 0 is least verbose, 2 is most verbose

-record(options, {
    action      = warn  :: action(),
    paths       = []    :: [file:filename()],
    includes    = []    :: [file:filename()],
    parse_beams = false :: boolean(),
    warn_errors = false :: boolean(),
    verbosity   = 1     :: verbosity()
}).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    atomizer_output:start(_Parent = self()),
    cli(CmdArgs),
    receive
        {halt, ExitCode} -> erlang:halt(ExitCode)
    end.

-spec cli([string()]) -> ok.
cli(CmdArgs) ->
    case parse_args(CmdArgs) of
        {options, Options} ->
            case run(Options) of
                {ok, ExitCode} ->
                    atomizer_output:halt(ExitCode);

                {error, {Module, Error}} ->
                    atomizer:error(Module:format_error(Error)),
                    atomizer_output:halt(2)
            end;

        {message, Message} ->
            atomizer:print(Message),
            atomizer_output:halt(0);

        {error, Error} ->
            atomizer:error(Error),
            atomizer_output:halt(2)
    end.

-spec parse_args([string()]) -> {options, #options{}} | {message, string()} | {error, string()}.
parse_args(CmdArgs) -> parse_args(CmdArgs, #options{}).

-spec parse_args([string()], #options{}) -> {options, #options{}} | {message, string()} | {error, string()}.
parse_args([], Options) -> {options, Options};
parse_args([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse_args(CmdArgs, Options#options{paths = [CmdArg | Options#options.paths]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_includes([string()], #options{}) -> {ok, #options{}} | {message, string()} | {error, string()}.
parse_includes([], Options) -> {options, Options};
parse_includes([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse_includes(CmdArgs, Options#options{includes = [CmdArg | Options#options.includes]});
        Option  -> parse_option(Option, CmdArgs, Options)
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

        _ParseBeams when Option == "b"; Option == "-parse-beams" ->
            parse_args(CmdArgs, Options#options{parse_beams = true});

        _Warnings when Option == "w"; Options == "-warn-errors" ->
            parse_args(CmdArgs, Options#options{warn_errors = true});

        _Verbosity when Option == "v"; Option == "-verbosity" ->
            case parse_verbosity(CmdArgs) of
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
    "Usage: atomizer [-a | --action ACTION] [-b | --parse-beams] [-i | -I | --include PATH*]\n" ++
    "                [-w | --warn-errors] [-v | --verbosity VERBOSITY] PATH*\n".

help() ->
    "".

-spec run(#options{} | file:filename()) -> {ok, ExitCode :: non_neg_integer()} | {error, term()}.
run(#options{action = Action} = Options) ->
    cli_init(Options),
    Package = package(Options),
    atomizer_progress:start(),
    case Action of
        list ->
            case atomizer_sup:collect_atoms(Package) of
                {ok, Atoms} -> list_atoms(Atoms);
                {error, Error} -> {error, Error}
            end;

        show ->
            case atomizer_sup:collect_atoms(Package) of
                {ok, Atoms} -> show_atoms(Atoms);
                {error, Error} -> {error, Error}
            end;

        warn ->
            case atomizer_sup:collect_warnings(Package) of
                {ok, Atoms, Warnings, NrFiles, NrDirs} -> warn_atoms(Atoms, Warnings, NrFiles, NrDirs);
                {error, Error} -> {error, Error}
            end
    end;

run(Path) ->
    run(#options{paths = [Path]}).

-spec run(action(), [file:filename()]) -> {ok, ExitCode :: non_neg_integer()} | {error, term()}.
run(Action, Paths) ->
    run(#options{action = Action, paths = Paths}).

-spec run(action(), [file:filename()], verbosity()) -> {ok, ExitCode :: non_neg_integer()} | {error, term()}.
run(Action, Paths, Verbosity) ->
    run(#options{action = Action, paths = Paths, verbosity = Verbosity}).

-spec cli_init(#options{}) -> ok.
cli_init(#options{warn_errors = WarnErrors, verbosity = Verbosity}) ->
    atomizer:cli_init(),
    atomizer:cli_set_warn_errors(WarnErrors),
    atomizer:cli_set_verbosity(Verbosity).

-spec package(#options{}) -> atomizer:package().
package(#options{paths = Paths, includes = Includes, parse_beams = ParseBeams}) ->
    atomizer:package(Paths, Includes, ParseBeams).

-spec list_atoms(atomizer:atoms()) -> {ok, ExitCode :: non_neg_integer()}.
list_atoms(Atoms) ->
    Verbosity = atomizer:cli_get_verbosity(),
    case lists:sort(maps:keys(Atoms)) of
        [] when Verbosity > 1 -> atomizer:print("No atoms found.");
        [] -> ok;
        Keys -> lists:foreach(fun (Atom) -> list_atom(Atom, maps:get(Atom, Atoms)) end, Keys)
    end,
    {ok, 0}.

-spec list_atom(atom(), atomizer:locations()) -> ok.
list_atom(Atom, Locations) ->
    NrOccurrences = atomizer:nr_occurrences(Locations),
    Columns = case atomizer:cli_get_verbosity() of
                  2 -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences), integer_to_list(atomizer:nr_files(Locations))];
                  _ -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences)]
              end,
    atomizer:print(lists:join("\t", Columns)).

-spec show_atoms(atomizer:atoms()) -> {ok, ExitCode :: non_neg_integer()}.
show_atoms(Atoms) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms)) end,
                  lists:sort(maps:keys(Atoms))),
    {ok, 0}.

-spec show_abridged_list(fun ((A) -> any()), [A]) -> {ok, ExitCode :: non_neg_integer()} when A :: term().
show_abridged_list(Printer, List) ->
    Verbosity = atomizer:cli_get_verbosity(),
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            atomizer:print(["... ", atomizer:italic(["(", integer_to_list(ListLength - PreviewLength), " more)"])]);
        _ ->
            lists:foreach(Printer, List)
    end,
    {ok, 0}.

-spec show_atom(atom(), atomizer:locations()) -> {ok, ExitCode :: non_neg_integer()}.
show_atom(Atom, Locations) -> show_atom(_Prompt = "", Atom, Locations).

-spec show_atom(io_lib:chars(), atom(), atomizer:locations()) -> {ok, ExitCode :: non_neg_integer()}.
show_atom(Prompt, Atom, Locations) ->
    atomizer:print(["\n", Prompt, atomizer:bold(atomizer:pretty_atom(Atom))]),
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    Files = lists:reverse(lists:keysort(3, Info)),
    ShowLocation = fun ({File, Positions, NrPositions}) -> show_location(File, Positions, NrPositions) end,
    show_abridged_list(ShowLocation, Files).

-spec show_location(file:filename(), [atomizer:position()], non_neg_integer()) -> {ok, ExitCode :: non_neg_integer()}.
show_location(File, Positions, NrPositions) ->
    case atomizer:cli_get_verbosity() of
        0 ->
            Occurrences = [integer_to_list(NrPositions), " ", atomizer:plural(NrPositions, "occurrence", "occurrences")],
            atomizer:print([File, " ", atomizer:italic(["(", Occurrences, ")"])]);
        _ ->
            ShowPosition = fun (Position) -> atomizer:print([File, ":" | show_position(Position)]) end,
            show_abridged_list(ShowPosition, Positions)
    end,
    {ok, 0}.

-spec show_position(atomizer:position()) -> io_lib:chars().
show_position(Line) when is_integer(Line) ->
    integer_to_list(Line);

show_position({Line, Column}) ->
    [integer_to_list(Line), ":", integer_to_list(Column)].

-spec warn_atoms(atomizer:atoms(), atomizer:warnings(), non_neg_integer(), non_neg_integer()) -> {ok, ExitCode :: non_neg_integer()}.
warn_atoms(Atoms, Warnings, NrFiles, NrDirs) ->
    lists:foreach(fun (Warning) -> warn_atom(Atoms, Warning) end, lists:sort(sets:to_list(Warnings))),
    NrAtoms    = maps:size(Atoms),
    NrWarnings = sets:size(Warnings),
    ThisManyLooseAtoms = pretty_quantity(NrWarnings, "loose atom", "loose atoms"),
    ThisManyAtoms      = pretty_quantity(NrAtoms,    "atom",       "atoms"),
    ThisManyFiles      = pretty_quantity(NrFiles,    "file",       "files"),
    ThisManyDirs       = pretty_quantity(NrDirs,     "directory",  "directories"),
    Message = ["Found", ThisManyLooseAtoms, "among", ThisManyAtoms, "in", ThisManyFiles, "and", ThisManyDirs],
    atomizer:print([atomizer:words(Message), "."]),
    case NrWarnings of
        0 -> {ok, 0};
        _ -> {ok, 1}
    end.

-spec pretty_quantity(non_neg_integer(), string(), string()) -> io_lib:chars().
pretty_quantity(Amount, Singular, Plural) ->
    [atomizer:bold(integer_to_list(Amount)), " ", atomizer:plural(Amount, Singular, Plural)].

-spec warn_atom(atomizer:atoms(), atomizer:warning()) -> ok.
warn_atom(Atoms, {A, B}) ->
    PrettyA = atomizer:bold(atomizer:pretty_atom(A)),
    PrettyB = atomizer:bold(atomizer:pretty_atom(B)),
    atomizer:print(atomizer:words([atomizer:cyan(PrettyA), atomizer:cyan("vs"), atomizer:cyan(PrettyB)])),
    show_atom("", A, maps:get(A, Atoms)),
    show_atom("Similar more frequent atom ", B, maps:get(B, Atoms)),
    atomizer:print("\n").
