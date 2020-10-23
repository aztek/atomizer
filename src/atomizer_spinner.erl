-module(atomizer_spinner).

-behavior(gen_server).

-export([
    start_link/1,
    show/1,
    tick/0,
    hide/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(TICK_INTERVAL, 200).
-define(SPINNER_PHASES, [$|, $/, $-, $\\]).

-type phase() :: 0..3.

-record(state, {
    banner          :: string() | undefined,
    ticks       = 0 :: non_neg_integer(),
    last_phase  = 0 :: phase(),
    last_ticked = 0 :: integer()
}).

-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(Banner) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Banner], []).

init([Banner]) ->
    {ok, #state{banner = Banner}}.

-spec show(string()) -> ok.
show(Banner) ->
    gen_server:cast(?MODULE, {show, Banner}).

-spec tick() -> ok.
tick() ->
    gen_server:cast(?MODULE, tick).

-spec hide() -> ok.
hide() ->
    gen_server:cast(?MODULE, hide).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({show, Banner}, _) ->
    {noreply, #state{banner = Banner}};

handle_cast(tick, State) when State#state.banner == undefined ->
    {noreply, State};

handle_cast(tick, State) ->
    #state{banner=Banner, ticks=Ticks, last_phase=LastPhase, last_ticked=LastTicked} = State,
    NowTicked = erlang:system_time(millisecond),
    case NowTicked - LastTicked >= ?TICK_INTERVAL of
        true ->
            NextPhase = (LastPhase + 1) rem length(?SPINNER_PHASES),
            atomizer_output:set_banner(render_spinner(Ticks, NextPhase, Banner)),
            {noreply, #state{banner = Banner, ticks = Ticks + 1, last_phase = NextPhase, last_ticked = NowTicked}};

        false ->
            {noreply, State#state{ticks = Ticks + 1}}
    end;

handle_cast(hide, _) ->
    atomizer_output:hide_banner(),
    {noreply, #state{banner = undefined}}.

-spec render_spinner(non_neg_integer(), phase(), string()) -> io_lib:chars().
render_spinner(Ticks, Phase, Format) ->
    Spinner = lists:nth(Phase + 1, ?SPINNER_PHASES),
    Label = io_lib:format(Format, [Ticks]),
    io_lib:format("[~c] ~s ", [Spinner, Label]).
