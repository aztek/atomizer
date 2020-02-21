-module(atomizer_progress).

-export([
    start/0,
    increase_total/1,
    increase_elapsed/1,
    finish/0
]).

-type progress() :: 0..100.

-define(PROCESS_NAME, progress_bar).

-define(PROGRESS_BAR_SCALE, 70).
-define(PROGRESS_BAR_WIDTH, ?PROGRESS_BAR_SCALE + 7).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn(fun () -> loop(0, 0, 0) end)).

-spec loop(non_neg_integer(), non_neg_integer(), progress()) -> no_return().
loop(Total, Elapsed, LastShownProgress) ->
    receive
        {increase, Amount, Delta} ->
            {IncreasedTotal, IncreasedElapsed} =
                case Amount of
                    total   -> {Total + Delta, Elapsed};
                    elapsed -> {Total, Elapsed + Delta}
                end,
            Progress =
                case IncreasedTotal of
                    0 -> 0;
                    _ -> erlang:floor(100 * IncreasedElapsed / IncreasedTotal)
                end,
            case Progress >= LastShownProgress of
                true ->
                    atomizer_output:set_progress(render_progress_bar(Progress)),
                    loop(IncreasedTotal, IncreasedElapsed, Progress);

                false ->
                    loop(IncreasedTotal, IncreasedElapsed, LastShownProgress)
            end;

        stop ->
            atomizer_output:hide_progress(),
            loop(Total, Elapsed, _LastShownProgress = 100)
    end.

-spec increase_total(non_neg_integer()) -> ok.
increase_total(Delta) ->
    ?PROCESS_NAME ! {increase, total, Delta},
    ok.

-spec increase_elapsed(non_neg_integer()) -> ok.
increase_elapsed(Delta) ->
    ?PROCESS_NAME ! {increase, elapsed, Delta},
    ok.

-spec render_progress_bar(progress()) -> string().
render_progress_bar(Progress) ->
    Elapsed = erlang:ceil(?PROGRESS_BAR_SCALE * Progress / 100),
    Bar = lists:duplicate(?PROGRESS_BAR_SCALE - Elapsed, $ ),
    Format = lists:flatten(io_lib:format("[~~~p..#s]~~3.w% ", [?PROGRESS_BAR_SCALE])),
    lists:flatten(io_lib:format(Format, [Bar, Progress])).

-spec finish() -> ok.
finish() ->
    ?PROCESS_NAME ! stop,
    ok.
