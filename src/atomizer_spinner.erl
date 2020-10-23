-module(atomizer_spinner).

-behavior(gen_server).

-export([
    start_link/1,
    start_link/2,
    activate/0,
    tick/0,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

-define(TICK_INTERVAL, 200).
-define(SPINNER_PHASES, [$|, $/, $-, $\\]).

-type phase() :: 0..3.

-record(state, {
    active :: boolean(),
    banner :: string(),
    ticks       = 0 :: non_neg_integer(),
    last_phase  = 0 :: phase(),
    last_ticked = 0 :: integer()
}).

-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(Banner) ->
    start_link(Banner, _Active = true).

-spec start_link(string(), boolean()) -> {ok, pid()} | {error, term()}.
start_link(Banner, Active) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Banner, Active], []).

init([Banner, Active]) ->
    {ok, #state{banner = Banner, active = Active}}.

-spec activate() -> ok.
activate() ->
    gen_server:cast(?MODULE, activate).

-spec tick() -> ok.
tick() ->
    gen_server:cast(?MODULE, tick).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(activate, State) ->
    {noreply, State#state{active = true}};

handle_cast(tick, State) ->
    #state{
        banner = Banner,
        active = Active,
        ticks = Ticks,
        last_phase = LastPhase,
        last_ticked = LastTicked
    } = State,
    NowTicked = erlang:system_time(millisecond),
    case NowTicked - LastTicked >= ?TICK_INTERVAL of
        true ->
            NextPhase = (LastPhase + 1) rem length(?SPINNER_PHASES),
            case Active of
                true  -> atomizer_output:set_banner(render_spinner(Ticks, NextPhase, Banner));
                false -> ignore
            end,
            {noreply, State#state{ticks = Ticks + 1, last_phase = NextPhase, last_ticked = NowTicked}};

        false ->
            {noreply, State#state{ticks = Ticks + 1}}
    end.

terminate(_, _) ->
    atomizer_output:hide_banner().

-spec render_spinner(non_neg_integer(), phase(), string()) -> io_lib:chars().
render_spinner(Ticks, Phase, Format) ->
    Spinner = lists:nth(Phase + 1, ?SPINNER_PHASES),
    Label = io_lib:format(Format, [Ticks]),
    io_lib:format("[~c] ~s ", [Spinner, Label]).
