-module(atomizer_compare).

-behavior(gen_server).

-export([
    start_link/0,
    atom/1,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

-record(state, {
    atoms :: ets:tid(),
    normal_forms :: ets:tid()
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{
        atoms = ets:new(atoms, [private, set]),
        normal_forms = ets:new(normal_forms, [private, bag])
    }}.

-spec atom(atom()) -> ok.
atom(Atom) ->
    gen_server:cast(?MODULE, {atom, Atom}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({atom, Atom}, State) ->
    case atomizer_heuristic:is_significant(Atom) andalso ets:insert_new(State#state.atoms, {Atom}) of
        true ->
            atomizer_spinner:tick(),
            case atomizer_heuristic:normalize(Atom) of
                {ok, NormalForm} ->
                    NormalForms = State#state.normal_forms,
                    case ets:lookup(NormalForms, NormalForm) of
                        [{_, Lookalike}] ->
                            atomizer_sup:lookalikes(Atom, Lookalike);
                        [] ->
                            ets:insert(NormalForms, {NormalForm, Atom})
                    end;
                nok -> ignore
            end;
        false -> ignore
    end,
    {noreply, State}.

terminate(_Reason, State) ->
    atomizer_spinner:hide(),
    ets:delete(State#state.normal_forms),
    ets:delete(State#state.atoms).
