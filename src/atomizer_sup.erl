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


-record(state, {
    action :: atomizer_cli_options:action(),
    atoms      = maps:new() :: atomizer:atoms(),
    lookalikes = sets:new() :: atomizer:lookalikes(),
    nr_parsed  = {-1, -1}   :: {integer(), integer()}
}).

-spec start(atomizer:package(), atomizer_cli_options:action()) -> true.
start(Package, Action) ->
    register(?PROCESS_NAME,
             spawn_link(fun () ->
                            atomizer_compare:start(),
                            atomizer_collect:start(Package),
                            Result = loop(#state{action = Action}),
                            atomizer_progress:stop(),
                            case Result of
                                {ok, Atoms, Stats} -> atomizer_cli:report(Atoms, Stats);
                                {error, Error} -> atomizer_cli:fail(Error)
                            end
                        end)).

-type result(A) :: {ok, [A], atomizer:statistics()} | {error, atomizer:error()}.

-spec loop(#state{}) -> result(atomizer:loose_atom()).
loop(State) ->
    receive
        {atom, Atom, File, Position} ->
            case State#state.action of
                warn -> atomizer_compare:atom(Atom);
                _    -> ignore
            end,
            Atoms = add_atom_location(Atom, File, Position, State#state.atoms),
            loop(State#state{atoms=Atoms});

        {done_atoms, NrFiles, NrDirs} when State#state.action == warn ->
            atomizer_compare:stop(),
            NrParsed = {NrFiles, NrDirs},
            loop(State#state{nr_parsed = NrParsed});

        {done_atoms, NrFiles, NrDirs} ->
            atomizer_compare:stop(),
            Atoms = State#state.atoms,
            SortedAtoms = [{Atom, maps:get(Atom, Atoms)} || Atom <- lists:sort(maps:keys(Atoms))],
            Stats = atomizer:statistics(_NrLooseAtoms = -1, _NrAtoms = length(SortedAtoms), NrFiles, NrDirs),
            {ok, SortedAtoms, Stats};

        {lookalikes, Atom, Lookalike} ->
            Lookalikes = sets:add_element({Atom, Lookalike}, State#state.lookalikes),
            loop(State#state{lookalikes = Lookalikes});

        done_comparing ->
            #state{atoms = Atoms, lookalikes = Lookalikes, nr_parsed = NrParsed} = State,
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
                    loop(State);
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
