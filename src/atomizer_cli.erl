-module(atomizer_cli).

-export([
    main/1
]).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    atomizer_output:start(_Parent = self()),
    ExitCode = cli(CmdArgs),
    atomizer_output:halt(ExitCode),
    receive
        {halt, ExitCode} -> erlang:halt(ExitCode)
    end.

-type exit_code() :: non_neg_integer().

-spec cli([string()]) -> exit_code().
cli(CmdArgs) ->
    case atomizer_cli_options:parse(CmdArgs) of
        {options, Options} ->
            case run(Options) of
                {ok, ExitCode} ->
                    ExitCode;

                {error, {Module, Error}} ->
                    atomizer:error(Module:format_error(Error)),
                    _Error = 2
            end;

        {message, Message} ->
            atomizer:print(Message),
            _OK = 0;

        {error, Error} ->
            atomizer:error(Error),
            _Error = 2
    end.

-spec run(atomizer_cli_options:options()) -> {ok, exit_code()} | {error, term()}.
run(Options) ->
    Package = atomizer_cli_options:package(Options),
    atomizer_cli_options:init(Options),
    atomizer_progress:start(),
    case atomizer_cli_options:get_action() of
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
    end.

-spec list_atoms(atomizer:atoms()) -> {ok, exit_code()}.
list_atoms(Atoms) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    case lists:sort(maps:keys(Atoms)) of
        [] when Verbosity > 1 -> atomizer:print("No atoms found.");
        [] -> ok;
        Keys -> lists:foreach(fun (Atom) -> list_atom(Atom, maps:get(Atom, Atoms)) end, Keys)
    end,
    {ok, 0}.

-spec list_atom(atom(), atomizer:locations()) -> ok.
list_atom(Atom, Locations) ->
    NrOccurrences = atomizer:nr_occurrences(Locations),
    Columns = case atomizer_cli_options:get_verbosity() of
                  2 -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences), integer_to_list(atomizer:nr_files(Locations))];
                  _ -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences)]
              end,
    atomizer:print(lists:join("\t", Columns)).

-spec show_atoms(atomizer:atoms()) -> {ok, exit_code()}.
show_atoms(Atoms) ->
    lists:foreach(fun (Atom) -> show_atom(Atom, maps:get(Atom, Atoms)) end,
                  lists:sort(maps:keys(Atoms))),
    {ok, 0}.

-spec show_abridged_list(fun ((A) -> any()), [A]) -> {ok, exit_code()} when A :: term().
show_abridged_list(Printer, List) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            atomizer:print(["... ", atomizer:italic(["(", integer_to_list(ListLength - PreviewLength), " more)"])]);
        _ ->
            lists:foreach(Printer, List)
    end,
    {ok, 0}.

-spec show_atom(atom(), atomizer:locations()) -> {ok, exit_code()}.
show_atom(Atom, Locations) -> show_atom(_Prompt = "", Atom, Locations).

-spec show_atom(io_lib:chars(), atom(), atomizer:locations()) -> {ok, exit_code()}.
show_atom(Prompt, Atom, Locations) ->
    atomizer:print(["\n", Prompt, atomizer:bold(atomizer:pretty_atom(Atom))]),
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    Files = lists:reverse(lists:keysort(3, Info)),
    ShowLocation = fun ({File, Positions, NrPositions}) -> show_location(File, Positions, NrPositions) end,
    show_abridged_list(ShowLocation, Files).

-spec show_location(file:filename(), [atomizer:position()], non_neg_integer()) -> {ok, exit_code()}.
show_location(File, Positions, NrPositions) ->
    case atomizer_cli_options:get_verbosity() of
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

-spec warn_atoms(atomizer:atoms(), atomizer:warnings(), non_neg_integer(), non_neg_integer()) -> {ok, exit_code()}.
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
