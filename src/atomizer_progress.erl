-module(atomizer_progress).

-export([
    start/0,
    tick/1,
    next/2,
    progress/1,
    finish/0
]).

-type phase() :: 0..3.
-type progress() :: 0..100.

-define(PROCESS_NAME, ?MODULE).

-define(TICK_INTERVAL, 200).
-define(SPINNER_PHASES, [$|, $/, $-, $\\]).

-define(PROGRESS_BAR_SCALE, 70).
-define(PROGRESS_BAR_WIDTH, ?PROGRESS_BAR_SCALE + 7).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn(fun () -> loop_spinner(0, 0, 0) end)).

-spec loop_spinner(non_neg_integer() | stop, phase(), non_neg_integer()) -> no_return().
loop_spinner(Ticks, LastPhase, LastTicked) ->
    receive
        {tick, _} when Ticks == stop ->
            loop_spinner(Ticks, LastPhase, LastTicked);

        {tick, Format} ->
            NowTicked = erlang:system_time(millisecond),
            case NowTicked - LastTicked >= ?TICK_INTERVAL of
                true ->
                    NextPhase = (LastPhase + 1) rem length(?SPINNER_PHASES),
                    atomizer_output:set_progress(render_spinner(Ticks, NextPhase, Format)),
                    loop_spinner(Ticks + 1, NextPhase, NowTicked);

                false ->
                    loop_spinner(Ticks + 1, LastPhase, LastTicked)
            end;

        {next, Elapsed, Total} ->
            loop_progress_bar(Total, Elapsed, _LastShownProgress = 0);

        stop ->
            atomizer_output:hide_progress(),
            loop_spinner(stop, LastPhase, LastTicked)
    end.

-spec loop_progress_bar(non_neg_integer(), non_neg_integer(), progress()) -> no_return().
loop_progress_bar(Total, Elapsed, LastShownProgress) ->
    receive
        {progress, Delta} ->
            IncreasedElapsed = Elapsed + Delta,
            IncreasedProgress = case Total of
                                    0 -> 100;
                                    _ -> erlang:floor(100 * IncreasedElapsed / Total)
                                end,
            Progress = erlang:min(100, IncreasedProgress),
            case Progress > LastShownProgress of
                true ->
                    atomizer_output:set_progress(render_progress_bar(Progress)),
                    loop_progress_bar(Total, IncreasedElapsed, Progress);

                false ->
                    loop_progress_bar(Total, IncreasedElapsed, LastShownProgress)
            end;

        stop ->
            atomizer_output:hide_progress(),
            loop_progress_bar(Total, Elapsed, _LastShownProgress = 100)
    end.

-spec tick(string()) -> ok.
tick(Format) ->
    ?PROCESS_NAME ! {tick, Format},
    ok.

-spec next(non_neg_integer(), non_neg_integer()) -> ok.
next(Elapsed, Total) ->
    ?PROCESS_NAME ! {next, Elapsed, Total},
    ok.

-spec progress(non_neg_integer()) -> ok.
progress(Delta) ->
    ?PROCESS_NAME ! {progress, Delta},
    ok.

-spec render_spinner(non_neg_integer(), phase(), string()) -> io_lib:chars().
render_spinner(Ticks, Phase, Format) ->
    Spinner = lists:nth(Phase + 1, ?SPINNER_PHASES),
    Label = io_lib:format(Format, [Ticks]),
    io_lib:format("[~c] ~s ", [Spinner, Label]).

-spec render_progress_bar(progress()) -> io_lib:chars().
render_progress_bar(Progress) ->
    Elapsed = erlang:ceil(?PROGRESS_BAR_SCALE * Progress / 100),
    Bar = lists:duplicate(?PROGRESS_BAR_SCALE - Elapsed, $ ),
    Format = lists:flatten(io_lib:format("[~~~p..#s]~~3.w% ", [?PROGRESS_BAR_SCALE])),
    io_lib:format(Format, [Bar, Progress]).

-spec finish() -> ok.
finish() ->
    ?PROCESS_NAME ! stop,
    ok.
