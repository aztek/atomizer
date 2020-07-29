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

                {error, Error} ->
                    atomizer:error(Error),
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
                {ok, Atoms, Stats} -> list_atoms(Atoms, Stats);
                {error, Error} -> {error, Error}
            end;

        show ->
            case atomizer_sup:collect_atoms(Package) of
                {ok, Atoms, Stats} -> show_atoms(Atoms, Stats);
                {error, Error} -> {error, Error}
            end;

        warn ->
            case atomizer_sup:find_loose_atoms(Package) of
                {ok, LooseAtoms, Stats} -> show_loose_atoms(LooseAtoms, Stats);
                {error, Error} -> {error, Error}
            end
    end.

-spec list_atoms([atomizer:atom_info()], atomizer:statistics()) -> {ok, exit_code()}.
list_atoms(Atoms, Stats) ->
    lists:foreach(fun list_atom/1, Atoms),
    Verbosity = atomizer_cli_options:get_verbosity(),
    if
        Verbosity > 1 -> show_statistics(Stats);
        true -> ok
    end,
    {ok, 0}.

-spec list_atom(atomizer:atom_info()) -> ok.
list_atom({Atom, Locations}) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    NrOccurrences = atomizer:nr_occurrences(Locations),
    NrFiles = atomizer:nr_files(Locations),
    Columns = [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences)] ++
              [integer_to_list(NrFiles) || Verbosity == 2],
    atomizer:print(lists:join("\t", Columns)).

-spec show_atoms([atomizer:atom_info()], atomizer:statistics()) -> {ok, exit_code()}.
show_atoms(Atoms, Stats) ->
    lists:foreach(fun show_atom/1, Atoms),
    Verbosity = atomizer_cli_options:get_verbosity(),
    if
        Verbosity > 1 -> show_statistics(Stats);
        true -> ok
    end,
    {ok, 0}.

-spec show_abridged_list(fun ((A) -> any()), [A]) -> {ok, exit_code()} when A :: term().
show_abridged_list(Printer, List) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            atomizer:print(["... ", atomizer_output:cyan(["(", integer_to_list(ListLength - PreviewLength), " more)"])]);
        _ ->
            lists:foreach(Printer, List)
    end,
    {ok, 0}.

-spec show_locations(atomizer:locations()) -> ok.
show_locations(Locations) ->
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    Files = lists:reverse(lists:keysort(3, Info)),
    ShowLocation = fun ({File, Positions, NrPositions}) -> show_location(File, Positions, NrPositions) end,
    show_abridged_list(ShowLocation, Files),
    atomizer:nl().

-spec show_location(file:filename(), [atomizer:position()], non_neg_integer()) -> {ok, exit_code()}.
show_location(File, Positions, NrPositions) ->
    case atomizer_cli_options:get_verbosity() of
        0 ->
            Occurrences = [integer_to_list(NrPositions), " ", atomizer:plural(NrPositions, "occurrence", "occurrences")],
            atomizer:print([File, " ", atomizer_output:cyan(["(", Occurrences, ")"])]);
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
    Found = case atomizer:get_nr_loose_atoms(Stats) >= 0 of
                true  -> ["Found", ThisManyLooseAtoms, "among"];
                false -> ["Found"]
            end,
    Message = Found ++ [ThisManyAtoms, "in", ThisManyFiles, "and", ThisManyDirs],
    atomizer:print([atomizer:words(Message), "."]).

-spec pretty_quantity(non_neg_integer(), string(), string()) -> io_lib:chars().
pretty_quantity(Amount, Singular, Plural) ->
    [atomizer_output:bold(integer_to_list(Amount)), " ", atomizer:plural(Amount, Singular, Plural)].

-spec show_atom(atomizer:atom_info()) -> ok.
show_atom({Atom, Locations}) ->
    atomizer:print(atomizer_output:bold(atomizer:pretty_atom(Atom))),
    show_locations(Locations).

-spec show_atom(atomizer:atom_info(), atom()) -> ok.
show_atom({Atom, Locations}, Lookalike) ->
    atomizer:print(atomizer_output:bold(show_difference(Atom, Lookalike))),
    show_locations(Locations).

-spec show_difference(atom(), atom()) -> io_lib:chars().
show_difference(Loose, Lookalike) ->
    show_difference_helper(atomizer:pretty_atom(Loose), atomizer:pretty_atom(Lookalike)).

-spec show_difference_helper(string(), string()) -> io_lib:chars().
show_difference_helper(Loose, []) -> Loose;
show_difference_helper([], _) -> [];
show_difference_helper([K | Loose], [C | Lookalike]) ->
    if
        K == C -> [K | show_difference_helper(Loose, Lookalike)];
        K == $_; K == $- -> [atomizer_output:bg_green([K]) | show_difference_helper(Loose, [C | Lookalike])];
        C == $_; C == $- -> show_difference_helper([K | Loose], Lookalike);
        true -> [atomizer_output:bg_yellow([K]) | show_difference_helper(Loose, Lookalike)]
    end.

-spec show_loose_atom(atomizer:loose_atom()) -> ok.
show_loose_atom({_Loose, _} = {LooseAtomInfo, {Lookalike, _} = LookalikeAtomInfo}) ->
    show_atom(LooseAtomInfo, Lookalike),
    show_atom(LookalikeAtomInfo),
    atomizer:nl().
