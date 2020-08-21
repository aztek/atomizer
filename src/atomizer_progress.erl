-module(atomizer_progress).

-export([
    start/2,
    progress/1,
    stop/0
]).

-type progress() :: 0..100.

-define(PROCESS_NAME, ?MODULE).

-define(PROGRESS_BAR_SCALE, 70).
-define(PROGRESS_BAR_WIDTH, ?PROGRESS_BAR_SCALE + 7).

-spec start(non_neg_integer(), non_neg_integer()) -> true.
start(Elapsed, Total) ->
    register(?PROCESS_NAME, spawn(fun () -> loop(Total, Elapsed, _LastShownProgress = 0) end)).

-spec loop(non_neg_integer(), non_neg_integer(), progress()) -> no_return().
loop(Total, Elapsed, LastShownProgress) ->
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
                    loop(Total, IncreasedElapsed, Progress);

                false ->
                    loop(Total, IncreasedElapsed, LastShownProgress)
            end;

        stop ->
            atomizer_output:hide_progress(),
            loop(Total, Elapsed, _LastShownProgress = 100)
    end.

-spec progress(non_neg_integer()) -> ok.
progress(Delta) ->
    ?PROCESS_NAME ! {progress, Delta},
    ok.

-spec stop() -> ok.
stop() ->
    ?PROCESS_NAME ! stop,
    ok.

-spec render_progress_bar(progress()) -> io_lib:chars().
render_progress_bar(Progress) ->
    Elapsed = erlang:ceil(?PROGRESS_BAR_SCALE * Progress / 100),
    Bar = lists:duplicate(?PROGRESS_BAR_SCALE - Elapsed, $ ),
    Format = lists:flatten(io_lib:format("[~~~p..#s]~~3.w% ", [?PROGRESS_BAR_SCALE])),
    io_lib:format(Format, [Bar, Progress]).
