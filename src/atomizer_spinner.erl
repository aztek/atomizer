-module(atomizer_spinner).

-export([
    start/0,
    tick/1,
    stop/0
]).

-type phase() :: 0..3.

-define(PROCESS_NAME, ?MODULE).

-define(TICK_INTERVAL, 200).
-define(SPINNER_PHASES, [$|, $/, $-, $\\]).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn(fun () -> loop(0, 0, 0) end)).

-spec loop(non_neg_integer() | stop, phase(), non_neg_integer()) -> no_return().
loop(Ticks, LastPhase, LastTicked) ->
    receive
        {tick, _} when Ticks == stop ->
            loop(Ticks, LastPhase, LastTicked);

        {tick, Format} ->
            NowTicked = erlang:system_time(millisecond),
            case NowTicked - LastTicked >= ?TICK_INTERVAL of
                true ->
                    NextPhase = (LastPhase + 1) rem length(?SPINNER_PHASES),
                    atomizer_output:set_progress(render_spinner(Ticks, NextPhase, Format)),
                    loop(Ticks + 1, NextPhase, NowTicked);

                false ->
                    loop(Ticks + 1, LastPhase, LastTicked)
            end;

        stop ->
            atomizer_output:hide_progress(),
            loop(stop, LastPhase, LastTicked)
    end.

-spec tick(string()) -> ok.
tick(Format) ->
    ?PROCESS_NAME ! {tick, Format},
    ok.

-spec stop() -> ok.
stop() ->
    ?PROCESS_NAME ! stop,
    ok.

-spec render_spinner(non_neg_integer(), phase(), string()) -> io_lib:chars().
render_spinner(Ticks, Phase, Format) ->
    Spinner = lists:nth(Phase + 1, ?SPINNER_PHASES),
    Label = io_lib:format(Format, [Ticks]),
    io_lib:format("[~c] ~s ", [Spinner, Label]).
