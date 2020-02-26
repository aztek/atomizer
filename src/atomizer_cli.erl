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
            case atomizer_sup:find_loose_atoms(Package) of
                {ok, LooseAtoms, Statistics} -> show_loose_atoms(LooseAtoms, Statistics);
                {error, Error} -> {error, Error}
            end
    end.

-spec list_atoms([atomizer:atom_info()]) -> {ok, exit_code()}.
list_atoms(Atoms) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    case Atoms of
        [] when Verbosity > 1 -> atomizer:print("No atoms found.");
        [] -> ok;
        _  -> lists:foreach(fun list_atom/1, Atoms)
    end,
    {ok, 0}.

-spec list_atom(atomizer:atom_info()) -> ok.
list_atom({Atom, Locations}) ->
    NrOccurrences = atomizer:nr_occurrences(Locations),
    Columns = case atomizer_cli_options:get_verbosity() of
                  2 -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences), integer_to_list(atomizer:nr_files(Locations))];
                  _ -> [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences)]
              end,
    atomizer:print(lists:join("\t", Columns)).

-spec show_atoms([atomizer:atom_info()]) -> {ok, exit_code()}.
show_atoms(Atoms) ->
    lists:foreach(fun show_atom/1, Atoms),
    {ok, 0}.

-spec show_abridged_list(fun ((A) -> any()), [A]) -> {ok, exit_code()} when A :: term().
show_abridged_list(Printer, List) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            atomizer:print(["... ", atomizer_output:italic(["(", integer_to_list(ListLength - PreviewLength), " more)"])]);
        _ ->
            lists:foreach(Printer, List)
    end,
    {ok, 0}.

-spec show_atom(atomizer:atom_info()) -> {ok, exit_code()}.
show_atom({Atom, Locations}) ->
    atomizer:print(["\n" | atomizer_output:bold(atomizer:pretty_atom(Atom))]),
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
            atomizer:print([File, " ", atomizer_output:italic(["(", Occurrences, ")"])]);
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

-spec show_loose_atoms([atomizer:loose_atom()], atomizer:statistics()) -> {ok, exit_code()}.
show_loose_atoms(LooseAtoms, Stats) ->
    lists:foreach(fun show_loose_atom/1, LooseAtoms),
    show_statistics(Stats),
    ExitCode = case LooseAtoms of [] -> 0; _ -> 1 end,
    {ok, ExitCode}.

-spec show_statistics(atomizer:statistics()) -> ok.
show_statistics(Stats) ->
    ThisManyLooseAtoms = pretty_quantity(atomizer:get_nr_loose_atoms(Stats), "loose atom", "loose atoms"),
    ThisManyAtoms      = pretty_quantity(atomizer:get_nr_atoms(Stats),       "atom",       "atoms"),
    ThisManyFiles      = pretty_quantity(atomizer:get_nr_files(Stats),       "file",       "files"),
    ThisManyDirs       = pretty_quantity(atomizer:get_nr_dirs(Stats),        "directory",  "directories"),
    Message = ["Found", ThisManyLooseAtoms, "among", ThisManyAtoms, "in", ThisManyFiles, "and", ThisManyDirs],
    atomizer:print([atomizer:words(Message), "."]).

-spec pretty_quantity(non_neg_integer(), string(), string()) -> io_lib:chars().
pretty_quantity(Amount, Singular, Plural) ->
    [atomizer_output:bold(integer_to_list(Amount)), " ", atomizer:plural(Amount, Singular, Plural)].

-spec show_loose_atom(atomizer:loose_atom()) -> ok.
show_loose_atom({{Loose, _} = LooseInfo, {Lookalike, _} = LookalikeInfo}) ->
    PrettyLoose     = atomizer_output:bold(atomizer:pretty_atom(Loose)),
    PrettyLookalike = atomizer_output:bold(atomizer:pretty_atom(Lookalike)),
    atomizer:print(atomizer_output:cyan(atomizer:words([PrettyLoose, "vs", PrettyLookalike]))),
    show_atom(LooseInfo),
    show_atom(LookalikeInfo),
    atomizer:print("\n").
