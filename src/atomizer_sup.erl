-module(atomizer_sup).

-behavior(gen_server).

-export([
    start/2,
    atom/3,
    lookalikes/2,
    done_atoms/2,
    fail/1,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {
    action :: atomizer_cli_options:action(),
    atoms = maps:new() :: atomizer:atoms(),
    lookalikes = sets:new() :: atomizer:lookalikes(),
    nr_parsed :: {integer(), integer()} | undefined
}).

init([Package, Action]) ->
    atomizer_spinner:start(),
    atomizer_collect:start(Package),
    case Action of
        warn -> atomizer_compare:start();
        _    -> ignore
    end,
    {ok, #state{action = Action}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({atom, Atom, File, Position}, State) ->
    case State#state.action of
        warn -> atomizer_compare:atom(Atom);
        _    -> ignore
    end,
    Atoms = add_atom_location(Atom, File, Position, State#state.atoms),
    {noreply, State#state{atoms=Atoms}};

handle_cast({done_atoms, NrFiles, NrDirs}, State) when State#state.action == warn ->
    atomizer_compare:stop(),
    {noreply, State#state{nr_parsed = {NrFiles, NrDirs}}};

handle_cast({done_atoms, NrFiles, NrDirs}, State) ->
    Atoms = State#state.atoms,
    SortedAtoms = [{Atom, maps:get(Atom, Atoms)} || Atom <- lists:sort(maps:keys(Atoms))],
    Stats = atomizer:statistics(_NrLooseAtoms = -1, _NrAtoms = length(SortedAtoms), NrFiles, NrDirs),
    atomizer_cli:report(SortedAtoms, Stats),
    {noreply, State};

handle_cast({lookalikes, Atom, Lookalike}, State) ->
    Lookalikes = sets:add_element({Atom, Lookalike}, State#state.lookalikes),
    {noreply, State#state{lookalikes = Lookalikes}};

handle_cast(done_comparing, State) ->
    atomizer_spinner:hide(),
    #state{atoms = Atoms, lookalikes = Lookalikes, nr_parsed = NrParsed} = State,
    LooseAtoms = lists:filtermap(fun ({A, B}) ->
                                     is_loose({A, maps:get(A, Atoms)}, {B, maps:get(B, Atoms)})
                                 end,
                                 sets:to_list(Lookalikes)),
    NrAtoms = maps:size(Atoms),
    NrLooseAtoms = length(LooseAtoms),
    {NrFiles, NrDirs} = NrParsed,
    Stats = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
    atomizer_cli:report(lists:sort(LooseAtoms), Stats),
    {noreply, State};

handle_cast({error, Error}, State) ->
    case atomizer_cli_options:get_warn_errors() of
        true  -> atomizer:warning(Error);
        false -> atomizer_cli:fail(Error)
    end,
    {noreply, State}.

-spec atom(atom(), file:filename(), atomizer:position()) -> ok.
atom(Atom, File, Position) ->
    gen_server:cast(?MODULE, {atom, Atom, File, Position}).

-spec lookalikes(atom(), atom()) -> ok.
lookalikes(Atom, Lookalike) ->
    gen_server:cast(?MODULE, {lookalikes, Atom, Lookalike}).

-spec done_atoms(NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()) -> ok.
done_atoms(NrFiles, NrDirs) ->
    atomizer_spinner:show("Searching for loose atoms (~p)"),
    gen_server:cast(?MODULE, {done_atoms, NrFiles, NrDirs}).

-spec fail(atomizer:error()) -> ok.
fail(Error) ->
    gen_server:cast(?MODULE, {error, Error}).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, done_comparing).

start(Package, Action) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Package, Action], []).

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
    Disproportional = Max > Min * Disproportion,

    case Disproportional andalso local(Loose) andalso related(A, B) of
        true  -> {true, {Loose, Lookalike}};
        false -> false
    end.

-spec local(atomizer:atom_info()) -> boolean().
local({_, Locations}) ->
    atomizer:nr_files(Locations) == 1.

-spec related(atomizer:atom_info(), atomizer:atom_info()) -> boolean().
related({_, LocationsA}, {_, LocationsB}) ->
    lists:any(fun (File) -> maps:is_key(File, LocationsB) end, maps:keys(LocationsA)).
