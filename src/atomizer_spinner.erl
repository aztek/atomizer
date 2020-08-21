-module(atomizer_spinner).

-behavior(gen_server).

-export([
    start/0,
    tick/1,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(TICK_INTERVAL, 200).
-define(SPINNER_PHASES, [$|, $/, $-, $\\]).

-type phase() :: 0..3.

-record(state, {
    ticks       = 0 :: non_neg_integer() | stop,
    last_phase  = 0 :: phase(),
    last_ticked = 0 :: integer()
}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec tick(string()) -> ok.
tick(Format) ->
    gen_server:cast(?MODULE, {tick, Format}).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({tick, _}, State) when State#state.ticks == stop ->
    {noreply, State};

handle_cast({tick, Format}, State) ->
    #state{ticks=Ticks, last_phase=LastPhase, last_ticked=LastTicked} = State,
    NowTicked = erlang:system_time(millisecond),
    case NowTicked - LastTicked >= ?TICK_INTERVAL of
        true ->
            NextPhase = (LastPhase + 1) rem length(?SPINNER_PHASES),
            atomizer_output:set_progress(render_spinner(Ticks, NextPhase, Format)),
            {noreply, #state{ticks = Ticks + 1, last_phase = NextPhase, last_ticked = NowTicked}};

        false ->
            {noreply, State#state{ticks = Ticks + 1}}
    end;

handle_cast(stop, State) ->
    atomizer_output:hide_progress(),
    {noreply, State#state{ticks=stop}}.

-spec render_spinner(non_neg_integer(), phase(), string()) -> io_lib:chars().
render_spinner(Ticks, Phase, Format) ->
    Spinner = lists:nth(Phase + 1, ?SPINNER_PHASES),
    Label = io_lib:format(Format, [Ticks]),
    io_lib:format("[~c] ~s ", [Spinner, Label]).
