-module(atomizer_compare).

-behavior(gen_server).

-export([
    start/0,
    atom/1,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

atom(Atom) ->
    gen_server:cast(?MODULE, {atom, Atom}).

stop() ->
    gen_server:cast(?MODULE, done_atoms).


init(_Args) ->
    Atoms = ets:new(atoms, [private, set]),
    NFs   = ets:new(nfs,   [private, bag]),
    {ok, {Atoms, NFs}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({atom, Atom}, State = {Atoms, NFs}) ->
    case insignificant(Atom) orelse ets:member(Atoms, Atom) of
        true -> ignore;
        false ->
            ets:insert(Atoms, {Atom}),
            lists:foreach(fun (NormalForm) ->
                              case ets:lookup(NFs, NormalForm) of
                                  [{_, Lookalike}] ->
                                      atomizer_sup:lookalikes(Atom, Lookalike);
                                  [] ->
                                      ets:insert(NFs, {NormalForm, Atom})
                              end
                          end,
                atomizer_normalize:normalize(Atom))
    end,
    {noreply, State};

handle_cast(done_atoms, State) ->
    atomizer_sup:stop(),
    {noreply, State}.

insignificant(Atom) ->
    length(atom_to_list(Atom)) =< 2.

terminate(_Reason, {Atoms, NFs}) ->
    ets:delete(NFs),
    ets:delete(Atoms).

