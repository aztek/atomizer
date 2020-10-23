-module(atomizer_sup).

-behavior(gen_server).

-export([
    start_link/2,
    file/1,
    done_dir/1,
    done_file/1,
    atom/3,
    lookalikes/2,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    package :: atomizer:package(),
    action :: atomizer_cli_options:action(),
    files = sets:new() :: sets:set(atomizer:file()),
    atoms = maps:new() :: atomizer:atoms(),
    lookalikes = sets:new() :: atomizer:lookalikes(),
    nr_dirs = 0 :: integer(),
    nr_files = 0 :: integer(),
    atomizer_traverse :: pid() | undefined,
    atomizer_parse    :: pid() | undefined,
    atomizer_compare  :: pid() | undefined
}).

-spec start_link(atomizer:package(), atomizer_cli_options:action()) -> {ok, pid()} | {error, term()}.
start_link(Package, Action) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Package, Action], []).

init([Package, Action]) ->
    process_flag(trap_exit, true),
    Paths = atomizer:package_paths(Package),
    case atomizer_traverse:detect_sources(Paths, Package) of
        {ok, Files, Dirs} ->
            {ok, Traverse} = atomizer_traverse:start_link(Dirs, Package),
            {ok, #state{
                package = Package,
                action = Action,
                files = sets:from_list(Files),
                atomizer_traverse = Traverse
            }};

        {error, Error} ->
            {stop, {shutdown, {error, Error}}}
    end.

-spec file(atomizer:file()) -> ok.
file(File) ->
    gen_server:cast(?MODULE, {file, File}).

-spec done_dir(file:filename()) -> ok.
done_dir(Dir) ->
    gen_server:cast(?MODULE, {done_dir, Dir}).

-spec done_file(atomizer:file()) -> ok.
done_file(File) ->
    gen_server:cast(?MODULE, {done_file, File}).

-spec atom(atom(), file:filename(), atomizer:position()) -> ok.
atom(Atom, File, Position) ->
    gen_server:cast(?MODULE, {atom, Atom, File, Position}).

-spec lookalikes(atom(), atom()) -> ok.
lookalikes(Atom, Lookalike) ->
    gen_server:cast(?MODULE, {lookalikes, Atom, Lookalike}).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({file, File}, State) ->
    {noreply, State#state{files = sets:add_element(File, State#state.files)}};

handle_cast({done_dir, _Dir}, State) ->
    {noreply, State#state{nr_dirs = State#state.nr_dirs + 1}};

handle_cast({done_file, _File}, State) ->
    {noreply, State#state{nr_files = State#state.nr_files + 1}};

handle_cast({atom, Atom, File, Position}, #state{action = warn} = State) ->
    atomizer_compare:atom(Atom),
    {noreply, State#state{atoms = add_atom_location(Atom, File, Position, State#state.atoms)}};

handle_cast({atom, Atom, File, Position}, State) ->
    {noreply, State#state{atoms = add_atom_location(Atom, File, Position, State#state.atoms)}};

handle_cast({lookalikes, Atom, Lookalike}, State) ->
    {noreply, State#state{lookalikes = sets:add_element({Atom, Lookalike}, State#state.lookalikes)}}.

handle_info({'EXIT', _Pid, {error, Error}}, State) ->
    {stop, {shutdown, {error, Error}}, State};

handle_info({'EXIT', Pid, normal}, #state{atomizer_traverse = Pid, action = warn} = State) ->
    {ok, Parse} = atomizer_parse:start_link(sets:to_list(State#state.files), State#state.package),
    {ok, Compare} = atomizer_compare:start_link(),
    {noreply, State#state{
        atomizer_traverse = undefined,
        atomizer_parse    = Parse,
        atomizer_compare  = Compare
    }};

handle_info({'EXIT', Pid, normal}, #state{atomizer_traverse = Pid} = State) ->
    {ok, Parse} = atomizer_parse:start_link(sets:to_list(State#state.files), State#state.package),
    {noreply, State#state{
        atomizer_traverse = undefined,
        atomizer_parse    = Parse
    }};

handle_info({'EXIT', Pid, normal}, #state{atomizer_parse = Pid, action = warn} = State) ->
    atomizer_compare:done_atoms(),
    {noreply, State#state{atomizer_parse = undefined}};

handle_info({'EXIT', Pid, normal}, #state{atomizer_parse = Pid} = State) ->
    {stop, {normal, {ok, atoms(State)}}, State#state{atomizer_parse = undefined}};

handle_info({'EXIT', Pid, normal}, #state{atomizer_compare = Pid} = State) ->
    {stop, {normal, {ok, loose_atoms(State)}}, State#state{atomizer_compare = undefined}};

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State}.

-spec atoms(#state{}) -> {atomizer:atoms(), atomizer:statistics()}.
atoms(#state{atoms = Atoms, nr_dirs = NrDirs, nr_files = NrFiles}) ->
    NrLooseAtoms = undefined,
    NrAtoms = maps:size(Atoms),
    Statistics = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
    {Atoms, Statistics}.

-spec loose_atoms(#state{}) -> {[atomizer:loose_atom()], atomizer:statistics()}.
loose_atoms(#state{atoms = Atoms, lookalikes = Lookalikes, nr_files = NrFiles, nr_dirs = NrDirs}) ->
    AtomInfo = fun (A) -> {A, maps:get(A, Atoms)} end,
    IsLoose = fun ({A, B}) -> atomizer_heuristic:is_loose(AtomInfo(A), AtomInfo(B)) end,
    LooseAtoms = lists:filtermap(IsLoose, sets:to_list(Lookalikes)),
    NrAtoms = maps:size(Atoms),
    NrLooseAtoms = length(LooseAtoms),
    Statistics = atomizer:statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs),
    {LooseAtoms, Statistics}.

-spec add_atom_location(atom(), file:filename(), atomizer:position(), atomizer:atoms()) -> atomizer:atoms().
add_atom_location(Atom, File, Position, Atoms) ->
    Locations = maps:get(Atom, Atoms, maps:new()),
    Positions = maps:get(File, Locations, sets:new()),
    UpdatedPositions = sets:add_element(Position, Positions),
    UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
    maps:put(Atom, UpdatedLocations, Atoms).
