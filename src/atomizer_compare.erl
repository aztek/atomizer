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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

atom(Atom) ->
    gen_server:cast(?MODULE, {atom, Atom}).

stop() ->
    gen_server:cast(?MODULE, done_atoms).

-record(state, {
    atoms :: ets:tid(),
    normal_forms :: ets:tid()
}).

init(_Args) ->
    {ok, #state{atoms = ets:new(atoms, [private, set]),
                normal_forms = ets:new(normal_forms, [private, bag])}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({atom, Atom}, State) ->
    case significant(Atom) andalso ets:insert_new(State#state.atoms, {Atom}) of
        true ->
            atomizer_spinner:tick(),
            case atomizer_normalize:normalize(Atom) of
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
    {noreply, State};

handle_cast(done_atoms, State) ->
    atomizer_sup:stop(),
    {noreply, State}.

significant(Atom) ->
    length(atom_to_list(Atom)) > 2.

terminate(_Reason, State) ->
    ets:delete(State#state.normal_forms),
    ets:delete(State#state.atoms).
