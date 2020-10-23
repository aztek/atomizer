-module(atomizer_cli).

-export([
    main/1
]).

-type exit_code() :: non_neg_integer().
-define(EXIT_CODE_SUCCESS, 0).
-define(EXIT_CODE_LOOSE_ATOMS_FOUND, 1).
-define(EXIT_CODE_FAILURE, 2).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
    process_flag(trap_exit, true),
    atomizer_output:start_link(),
    run(CmdArgs),
    ExitCode = wait(),
    atomizer_output:stop(),
    erlang:halt(ExitCode).

-spec run([string()]) -> ok.
run(CmdArgs) ->
    case atomizer_cli_options:parse(CmdArgs) of
        {options, Options} ->
            Package = atomizer_cli_options:package(Options),
            atomizer_cli_options:init(Options),
            Action = atomizer_cli_options:get_action(),
            atomizer_sup:start_link(Package, Action);

        {message, Message} ->
            atomizer:print(Message),
            erlang:halt(?EXIT_CODE_SUCCESS);

        {error, Error} ->
            atomizer:error(Error),
            erlang:halt(?EXIT_CODE_FAILURE)
    end.

-spec wait() -> exit_code().
wait() ->
    receive
        {'EXIT', _Pid, {shutdown, {ok, {Atoms, Stats}}}} ->
            case atomizer_cli_options:get_action() of
                list -> list_atoms(Atoms, Stats);
                show -> show_atoms(Atoms, Stats);
                warn -> show_loose_atoms(Atoms, Stats)
            end;

        {'EXIT', _Pid, {shutdown, {error, Error}}} ->
            atomizer:error(Error),
            ?EXIT_CODE_FAILURE
    end.

-spec list_atoms(atomizer:atoms(), atomizer:statistics()) -> exit_code().
list_atoms(Atoms, Stats) ->
    lists:foreach(fun (Atom) -> list_atom(Atom, maps:get(Atom, Atoms)) end,
                  lists:sort(maps:keys(Atoms))),
    Verbosity = atomizer_cli_options:get_verbosity(),
    if
        Verbosity > 1 -> show_statistics(Stats);
        true -> ok
    end,
    ?EXIT_CODE_SUCCESS.

-spec list_atom(atom(), atomizer:locations()) -> ok.
list_atom(Atom, Locations) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    NrOccurrences = atomizer:nr_occurrences(Locations),
    NrFiles = atomizer:nr_files(Locations),
    Columns = [atomizer:pretty_atom(Atom), integer_to_list(NrOccurrences)] ++
              [integer_to_list(NrFiles) || Verbosity == 2],
    atomizer:print(lists:join("\t", Columns)).

-spec show_atoms(atomizer:atoms(), atomizer:statistics()) -> exit_code().
show_atoms(Atoms, Stats) ->
    lists:foreach(fun (Atom) -> show_atom({Atom, maps:get(Atom, Atoms)}) end,
                  lists:sort(maps:keys(Atoms))),
    Verbosity = atomizer_cli_options:get_verbosity(),
    if
        Verbosity > 1 -> show_statistics(Stats);
        true -> ok
    end,
    ?EXIT_CODE_SUCCESS.

-spec show_abridged_list(fun ((A) -> any()), [A]) -> ok when A :: term().
show_abridged_list(Printer, List) ->
    Verbosity = atomizer_cli_options:get_verbosity(),
    PreviewLength = 4,
    case length(List) of
        ListLength when Verbosity =< 1, ListLength > PreviewLength + 1 ->
            lists:foreach(Printer, lists:sublist(List, PreviewLength)),
            atomizer:print(["... ", atomizer_color:cyan(["(", integer_to_list(ListLength - PreviewLength), " more)"])]);
        _ ->
            lists:foreach(Printer, List)
    end.

-spec show_locations(atomizer:locations()) -> ok.
show_locations(Locations) ->
    Info = [{filename:absname(File), lists:sort(sets:to_list(Positions)), sets:size(Positions)} ||
            {File, Positions} <- maps:to_list(Locations)],
    Files = lists:reverse(lists:keysort(3, Info)),
    ShowLocation = fun ({File, Positions, NrPositions}) -> show_location(File, Positions, NrPositions) end,
    show_abridged_list(ShowLocation, Files),
    atomizer:nl().

-spec show_location(file:filename(), [atomizer:position()], non_neg_integer()) -> ok.
show_location(File, Positions, NrPositions) ->
    case atomizer_cli_options:get_verbosity() of
        0 ->
            Occurrences = [integer_to_list(NrPositions), " ", atomizer:plural(NrPositions, "occurrence", "occurrences")],
            atomizer:print([File, " ", atomizer_color:cyan(["(", Occurrences, ")"])]);
        _ ->
            ShowPosition = fun (Position) -> atomizer:print([File, ":" | show_position(Position)]) end,
            show_abridged_list(ShowPosition, Positions)
    end.

-spec show_position(atomizer:position()) -> io_lib:chars().
show_position(Line) when is_integer(Line) ->
    integer_to_list(Line);

show_position({Line, Column}) ->
    [integer_to_list(Line), ":", integer_to_list(Column)].

-spec show_loose_atoms([atomizer:loose_atom()], atomizer:statistics()) -> exit_code().
show_loose_atoms(LooseAtoms, Stats) ->
    lists:foreach(fun show_loose_atom/1, lists:sort(LooseAtoms)),
    show_statistics(Stats),
    case LooseAtoms of
        [] -> ?EXIT_CODE_SUCCESS;
        _  -> ?EXIT_CODE_LOOSE_ATOMS_FOUND
    end.

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
    [atomizer_color:bold(integer_to_list(Amount)), " ", atomizer:plural(Amount, Singular, Plural)].

-spec show_atom(atomizer:atom_info()) -> ok.
show_atom({Atom, Locations}) ->
    atomizer:print(atomizer_color:bold(atomizer:pretty_atom(Atom))),
    show_locations(Locations).

-spec show_atom(atomizer:atom_info(), atom()) -> ok.
show_atom({Atom, Locations}, Lookalike) ->
    atomizer:print(atomizer_color:bold(show_difference(Atom, Lookalike))),
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
        K == $_; K == $- -> [atomizer_color:grey(atomizer_color:bg_green([K])) | show_difference_helper(Loose, [C | Lookalike])];
        C == $_; C == $- -> show_difference_helper([K | Loose], Lookalike);
        true -> [atomizer_color:grey(atomizer_color:bg_yellow([K])) | show_difference_helper(Loose, Lookalike)]
    end.

-spec show_loose_atom(atomizer:loose_atom()) -> ok.
show_loose_atom({_Loose, _} = {LooseAtomInfo, {Lookalike, _} = LookalikeAtomInfo}) ->
    show_atom(LooseAtomInfo, Lookalike),
    show_atom(LookalikeAtomInfo),
    atomizer:print(lists:duplicate(80, $=)),
    atomizer:nl().
