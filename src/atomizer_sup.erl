-module(atomizer_sup).

-export([
    collect_atoms/1,
    collect_warnings/1,
    format_error/1
]).

-type atoms_result() :: {ok, atomizer:atoms()} | {error, {?MODULE, term()}}.

-spec collect_atoms(atomizer:package()) -> atoms_result().
collect_atoms(Package) ->
    spawn_link(atomizer_collect, collect, [self(), Package]),
    atomizer_progress:start(),
    Result = collect_atoms_loop(maps:new()),
    atomizer_progress:finish(),
    Result.

-spec collect_atoms_loop(atomizer:atoms()) -> atoms_result().
collect_atoms_loop(Atoms) ->
    receive
        {atom, Atom, File, Position} ->
            collect_atoms_loop(add_atom_location(Atom, File, Position, Atoms));

        {done_atoms, _NrFiles, _NrDirs} ->
            {ok, Atoms};

        {error, Error} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(format_error(Error)),
                    collect_atoms_loop(Atoms);
                false ->
                    {error, Error}
            end
    end.

-type warnings_result() :: {ok, atomizer:atoms(), atomizer:warnings(), atomizer:statistics()}
                         | {error, {?MODULE, term()}}.

-spec collect_warnings(atomizer:package()) -> warnings_result().
collect_warnings(Package) ->
    Pid = spawn_link(atomizer_compare, compare, [self()]),
    spawn_link(atomizer_collect, collect, [self(), Package]),
    atomizer_progress:start(),
    Result = collect_warnings_loop(Pid, maps:new(), sets:new(), {-1, -1}),
    atomizer_progress:finish(),
    Result.

-spec collect_warnings_loop(pid(), atomizer:atoms(), atomizer:warnings(), {integer(), integer()}) -> warnings_result().
collect_warnings_loop(Pid, Atoms, Warnings, NrParsed) ->
    receive
        {atom, Atom, File, Position} ->
            Pid ! {atom, Atom},
            collect_warnings_loop(Pid, add_atom_location(Atom, File, Position, Atoms), Warnings, NrParsed);

        {done_atoms, NrFiles, NrDirs} ->
            Pid ! done_atoms,
            collect_warnings_loop(Pid, Atoms, Warnings, {NrFiles, NrDirs});

        {warning, Atom, Btom, _} ->
            collect_warnings_loop(Pid, Atoms, sets:add_element({Atom, Btom}, Warnings), NrParsed);

        done_warnings ->
            SignificantWarnings = sets:filter(fun (Warning) -> is_significant(Atoms, Warning) end, Warnings),
            NrAtoms = maps:size(Atoms),
            NrLooseAtoms = sets:size(SignificantWarnings),
            {NrFiles, NrDirs} = NrParsed,
            Stats = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
            {ok, Atoms, SignificantWarnings, Stats};

        {error, Error} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(format_error(Error)),
                    collect_warnings_loop(Pid, Atoms, Warnings, NrParsed);
                false ->
                    {error, Error}
            end
    end.

-spec add_atom_location(atom(), file:filename(), atomizer:position(), atomizer:atoms()) -> atomizer:atoms().
add_atom_location(Atom, File, Position, Atoms) ->
    Locations = maps:get(Atom, Atoms, maps:new()),
    Positions = maps:get(File, Locations, sets:new()),
    UpdatedPositions = sets:add_element(Position, Positions),
    UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
    maps:put(Atom, UpdatedLocations, Atoms).

-spec is_significant(atomizer:atoms(), atomizer:warning()) -> boolean().
is_significant(Atoms, {A, B}) ->
    LocationsA = maps:get(A, Atoms),
    LocationsB = maps:get(B, Atoms),
    NrOccurrenceA = atomizer:nr_occurrences(LocationsA),
    NrOccurrenceB = atomizer:nr_occurrences(LocationsB),
    {Typo, Min, Max} =
    if
        NrOccurrenceA < NrOccurrenceB -> {A, NrOccurrenceA, NrOccurrenceB};
        true -> {B, NrOccurrenceB, NrOccurrenceA}
    end,
    Disproportion = 4,
    Disproportional = Max / Min > Disproportion,
    Local = atomizer:nr_files(maps:get(Typo, Atoms)) == 1,
    Related = not sets:is_disjoint(sets:from_list(maps:keys(LocationsA)), sets:from_list(maps:keys(LocationsB))),
    Disproportional andalso Local andalso Related.

-spec format_error({module(), term()}) -> io_lib:chars().
format_error({Module, Error}) ->
    Module:format_error(Error).
