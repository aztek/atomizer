-module(atomizer_sup).

-behavior(gen_server).

-export([
    start/2,
    file/1,
    done_dir/1,
    done_file/1,
    done_files/0,
    atom/3,
    lookalikes/2,
    done_atoms/0,
    fail/1,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {
    package :: atomizer:package(),
    action :: atomizer_cli_options:action(),
    files = sets:new() :: sets:set(atomizer:file()),
    atoms = maps:new() :: atomizer:atoms(),
    lookalikes = sets:new() :: atomizer:lookalikes(),
    nr_dirs = 0 :: integer(),
    nr_files = 0 :: integer()
}).

init([Package, Action]) ->
    atomizer_spinner:start(),
    atomizer_spinner:show("Collecting files and directories (~p)"),
    Paths = atomizer:package_paths(Package),
    case atomizer_traverse:detect_sources(Paths, Package) of
        {ok, Sources} ->
            {Files, Dirs} = split_sources(Sources),
            atomizer_traverse:start(Dirs, Package),
            case Action of
                warn -> atomizer_compare:start();
                _    -> ignore
            end,
            {ok, #state{package = Package, action = Action, files = sets:from_list(Files)}};

        {error, Error} ->
            {stop, Error}
    end.

split_sources([]) -> {[], []};
split_sources([Source | Sources]) ->
    {Files, Dirs} = split_sources(Sources),
    case Source of
        {dir, Dir} -> {Files, [Dir | Dirs]};
        File -> {[File | Files], Dirs}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({file, File}, State) ->
    {noreply, State#state{files = sets:add_element(File, State#state.files)}};

handle_cast({done_dir, _Dir}, State) ->
    {noreply, State#state{nr_dirs = State#state.nr_dirs + 1}};

handle_cast({done_file, _File}, State) ->
    {noreply, State#state{nr_files = State#state.nr_files + 1}};

handle_cast(done_traversing, #state{files = Files, package = Package} = State) ->
    atomizer_progress:start(_Elapsed = 0, _Total = sets:size(Files)),
    atomizer_parse:start(sets:to_list(Files), Package),
    {noreply, State};

handle_cast({atom, Atom, File, Position}, State) ->
    case State#state.action of
        warn -> atomizer_compare:atom(Atom);
        _    -> ignore
    end,
    Atoms = add_atom_location(Atom, File, Position, State#state.atoms),
    {noreply, State#state{atoms = Atoms}};

handle_cast(done_atoms, State) when State#state.action == warn ->
    atomizer_progress:stop(),
    atomizer_compare:stop(),
    {noreply, State};

handle_cast(done_atoms, #state{atoms = Atoms, nr_dirs = NrDirs, nr_files = NrFiles} = State) ->
    Stats = atomizer:statistics(_NrLooseAtoms = undefined, _NrAtoms = maps:size(Atoms), NrFiles, NrDirs),
    atomizer_cli:report(Atoms, Stats),
    {noreply, State};

handle_cast({lookalikes, Atom, Lookalike}, State) ->
    Lookalikes = sets:add_element({Atom, Lookalike}, State#state.lookalikes),
    {noreply, State#state{lookalikes = Lookalikes}};

handle_cast(done_comparing, State) ->
    atomizer_spinner:hide(),
    #state{atoms = Atoms, lookalikes = Lookalikes, nr_files = NrFiles, nr_dirs = NrDirs} = State,
    LooseAtoms = lists:filtermap(fun ({A, B}) ->
                                     is_loose({A, maps:get(A, Atoms)}, {B, maps:get(B, Atoms)})
                                 end,
                                 sets:to_list(Lookalikes)),
    NrAtoms = maps:size(Atoms),
    NrLooseAtoms = length(LooseAtoms),
    Stats = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
    atomizer_cli:report(LooseAtoms, Stats),
    {noreply, State};

handle_cast({error, Error}, State) ->
    case atomizer_cli_options:get_warn_errors() of
        true  -> atomizer:warning(Error);
        false -> atomizer_cli:fail(Error)
    end,
    {noreply, State}.

file(File) ->
    atomizer_spinner:tick(),
    gen_server:cast(?MODULE, {file, File}).

done_dir(Dir) ->
    atomizer_spinner:tick(),
    gen_server:cast(?MODULE, {done_dir, Dir}).

done_file(File) ->
    atomizer_progress:progress(1),
    gen_server:cast(?MODULE, {done_file, File}).

done_files() ->
    atomizer_spinner:hide(),
    gen_server:cast(?MODULE, done_traversing).

-spec atom(atom(), file:filename(), atomizer:position()) -> ok.
atom(Atom, File, Position) ->
    gen_server:cast(?MODULE, {atom, Atom, File, Position}).

-spec lookalikes(atom(), atom()) -> ok.
lookalikes(Atom, Lookalike) ->
    gen_server:cast(?MODULE, {lookalikes, Atom, Lookalike}).

-spec done_atoms() -> ok.
done_atoms() ->
    atomizer_spinner:show("Searching for loose atoms (~p)"),
    gen_server:cast(?MODULE, done_atoms).

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
