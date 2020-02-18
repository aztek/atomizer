-module(atomizer).

-include("atomizer.hrl").

-export([
    collect_atoms/3,
    collect_warnings/3,
    format_error/1
]).

-type atoms_result() :: {ok, atoms()} | {error, {?MODULE, term()}}.

-spec collect_atoms([file:filename()], [file:filename()], boolean()) -> atoms_result().
collect_atoms(Paths, IncludePaths, ParseBeams) ->
    spawn_link(atomizer_collect, collect, [self(), Paths, IncludePaths, ParseBeams]),
    collect_atoms_loop(maps:new()).

-spec collect_atoms_loop(atoms()) -> atoms_result().
collect_atoms_loop(Atoms) ->
    receive
        {atom, Atom, File, Position} ->
            collect_atoms_loop(add_atom_location(Atom, File, Position, Atoms));

        {done_atoms, _NrFiles, _NrDirs} ->
            {ok, Atoms};

        {error, Error} ->
            case ?WARN_ERRORS of
                true ->
                    ?WARNING(format_error(Error)),
                    collect_atoms_loop(Atoms);
                false ->
                    {error, Error}
            end
    end.

-type warnings_result() :: {ok, atoms(), warnings(), NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()}
                         | {error, {?MODULE, term()}}.

-spec collect_warnings([file:filename()], [file:filename()], boolean()) -> warnings_result().
collect_warnings(Paths, IncludePaths, ParseBeams) ->
    Pid = spawn_link(atomizer_compare, compare, [self()]),
    spawn_link(atomizer_collect, collect, [self(), Paths, IncludePaths, ParseBeams]),
    collect_warnings_loop(Pid, maps:new(), sets:new(), {-1, -1}).

-spec collect_warnings_loop(pid(), atoms(), warnings(), {integer(), integer()}) -> warnings_result().
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
            {NrFiles, NrDirs} = NrParsed,
            SignificantWarnings = sets:filter(fun (Warning) -> is_significant(Atoms, Warning) end, Warnings),
            {ok, Atoms, SignificantWarnings, NrFiles, NrDirs};

        {error, Error} ->
            case ?WARN_ERRORS of
                true ->
                    ?WARNING(format_error(Error)),
                    collect_warnings_loop(Pid, Atoms, Warnings, NrParsed);
                false ->
                    {error, Error}
            end
    end.

-spec add_atom_location(atom(), file:filename(), position(), atoms()) -> atoms().
add_atom_location(Atom, File, Position, Atoms) ->
    Locations = maps:get(Atom, Atoms, maps:new()),
    Positions = maps:get(File, Locations, sets:new()),
    UpdatedPositions = sets:add_element(Position, Positions),
    UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
    maps:put(Atom, UpdatedLocations, Atoms).

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

-spec format_error({module(), term()}) -> string().
format_error({Module, Error}) ->
    Module:format_error(Error).
