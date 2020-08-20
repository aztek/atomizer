-module(atomizer_sup).

-export([
    start/2,
    atom/3,
    lookalikes/2,
    done_atoms/2,
    fail/1,
    stop/0
]).

-define(PROCESS_NAME, ?MODULE).

-spec atom(atom(), file:filename(), atomizer:position()) -> ok.
atom(Atom, File, Position) ->
    ?PROCESS_NAME ! {atom, Atom, File, Position},
    ok.

-spec lookalikes(atom(), atom()) -> ok.
lookalikes(Atom, Lookalike) ->
    ?PROCESS_NAME ! {lookalikes, Atom, Lookalike},
    ok.

-spec done_atoms(NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()) -> ok.
done_atoms(NrFiles, NrDirs) ->
    ?PROCESS_NAME ! {done_atoms, NrFiles, NrDirs},
    ok.

-spec fail(atomizer:error()) -> ok.
fail(Error) ->
    ?PROCESS_NAME ! {error, Error},
    ok.

-spec stop() -> ok.
stop() ->
    ?PROCESS_NAME ! done_comparing,
    ok.

-spec start(atomizer:package(), atomizer_cli_options:action()) -> true.
start(Package, Action) ->
    register(?PROCESS_NAME,
             spawn_link(fun () ->
                            atomizer_progress:start(),
                            atomizer_compare:start(),
                            atomizer_collect:start(Package),
                            Result = case Action of
                                         warn  -> find_loose_atoms_loop(maps:new(), sets:new(), {-1, -1});
                                         _Show -> collect_atoms_loop(maps:new())
                                     end,
                            atomizer_progress:finish(),
                            case Result of
                                {ok, Atoms, Stats} -> atomizer_cli:report(Atoms, Stats);
                                {error, Error} -> atomizer_cli:fail(Error)
                            end
                        end)).

-type result(A) :: {ok, [A], atomizer:statistics()} | {error, atomizer:error()}.

-spec collect_atoms_loop(atomizer:atoms()) -> result(atomizer:atom_info()).
collect_atoms_loop(Atoms) ->
    receive
        {atom, Atom, File, Position} ->
            collect_atoms_loop(add_atom_location(Atom, File, Position, Atoms));

        {done_atoms, NrFiles, NrDirs} ->
            atomizer_compare:stop(),
            SortedAtoms = [{Atom, maps:get(Atom, Atoms)} || Atom <- lists:sort(maps:keys(Atoms))],
            Stats = atomizer:statistics(_NrLooseAtoms = -1, _NrAtoms = length(SortedAtoms), NrFiles, NrDirs),
            {ok, SortedAtoms, Stats};

        {error, Error} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(Error),
                    collect_atoms_loop(Atoms);
                false ->
                    {error, Error}
            end
    end.

-spec find_loose_atoms_loop(atomizer:atoms(), atomizer:lookalikes(), {integer(), integer()}) ->
    result(atomizer:loose_atom()).
find_loose_atoms_loop(Atoms, Lookalikes, NrParsed) ->
    receive
        {atom, Atom, File, Position} ->
            atomizer_compare:atom(Atom),
            find_loose_atoms_loop(add_atom_location(Atom, File, Position, Atoms), Lookalikes, NrParsed);

        {done_atoms, NrFiles, NrDirs} ->
            atomizer_compare:stop(),
            find_loose_atoms_loop(Atoms, Lookalikes, {NrFiles, NrDirs});

        {lookalikes, Atom, Lookalike} ->
            find_loose_atoms_loop(Atoms, sets:add_element({Atom, Lookalike}, Lookalikes), NrParsed);

        done_comparing ->
            LooseAtoms = lists:filtermap(fun ({A, B}) ->
                                             is_loose({A, maps:get(A, Atoms)}, {B, maps:get(B, Atoms)})
                                         end,
                                         sets:to_list(Lookalikes)),
            NrAtoms = maps:size(Atoms),
            NrLooseAtoms = length(LooseAtoms),
            {NrFiles, NrDirs} = NrParsed,
            Stats = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
            {ok, lists:sort(LooseAtoms), Stats};

        {error, Error} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(Error),
                    find_loose_atoms_loop(Atoms, Lookalikes, NrParsed);
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

-spec is_loose(atomizer:atom_info(), atomizer:atom_info()) -> {true, atomizer:loose_atom()} | false.
is_loose({_, LocationsA} = A, {_, LocationsB} = B) ->
    NrOccurrencesA = atomizer:nr_occurrences(LocationsA),
    NrOccurrencesB = atomizer:nr_occurrences(LocationsB),

    {{Loose, Lookalike}, Min, Max} =
        case NrOccurrencesA < NrOccurrencesB of
            true  -> {{A, B}, NrOccurrencesA, NrOccurrencesB};
            false -> {{B, A}, NrOccurrencesB, NrOccurrencesA}
        end,

    Disproportion = 4,
    Disproportional = Max / Min > Disproportion,

    case Disproportional andalso local(Loose) andalso related(A, B) of
        true  -> {true, {Loose, Lookalike}};
        false -> false
    end.

-spec local(atomizer:atom_info()) -> boolean().
local({_, Locations}) ->
    atomizer:nr_files(Locations) == 1.

-spec related(atomizer:atom_info(), atomizer:atom_info()) -> boolean().
related({_, LocationsA}, {_, LocationsB}) ->
    FilesA = sets:from_list(maps:keys(LocationsA)),
    FilesB = sets:from_list(maps:keys(LocationsB)),
    not sets:is_disjoint(FilesA, FilesB).
