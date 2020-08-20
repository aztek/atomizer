-module(atomizer_output).

-export([
    start/0,
    put_chars/2,
    set_progress/1,
    hide_progress/0,
    stop/0
]).

-define(PROCESS_NAME, ?MODULE).

-define(PROGRESS_BAR_DEVICE, standard_error).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn(fun () -> loop() end)).

-spec loop() -> ok.
loop() ->
    receive
        {output, IoDevice, Message} ->
            io:put_chars(IoDevice, Message),
            loop();

        {set_progress, ProgressBar} ->
            draw_progress_bar(ProgressBar),
            loop_progress(ProgressBar);

        hide_progress ->
            loop();

        stop ->
            atomizer_cli:stop()
    end.

-spec loop_progress(string()) -> ok.
loop_progress(LastShownProgressBar) ->
    receive
        {output, IoDevice, Message} ->
            erase_progress_bar(LastShownProgressBar),
            io:put_chars(IoDevice, Message),
            draw_progress_bar(LastShownProgressBar),
            loop_progress(LastShownProgressBar);

        {set_progress, ProgressBar} when ProgressBar == LastShownProgressBar ->
            loop_progress(LastShownProgressBar);

        {set_progress, ProgressBar} ->
            redraw_progress_bar(LastShownProgressBar, ProgressBar),
            loop_progress(ProgressBar);

        hide_progress ->
            erase_progress_bar(LastShownProgressBar),
            loop();

        stop ->
            erase_progress_bar(LastShownProgressBar),
            atomizer_cli:stop()
    end.

-spec put_chars(io:device(), io_lib:chars()) -> ok.
put_chars(IoDevice, Message) ->
    ?PROCESS_NAME ! {output, IoDevice, Message},
    ok.

-spec set_progress(string()) -> ok.
set_progress(ProgressBar) ->
    ?PROCESS_NAME ! {set_progress, ProgressBar},
    ok.

-spec hide_progress() -> ok.
hide_progress() ->
    ?PROCESS_NAME ! hide_progress,
    ok.

-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-spec draw_progress_bar(io_lib:chars()) -> ok.
draw_progress_bar(ProgressBar) ->
    io:put_chars(?PROGRESS_BAR_DEVICE, [?HIDE_CURSOR, ProgressBar, ?SHOW_CURSOR]).

-spec stop() -> ok.
stop() ->
    ?PROCESS_NAME ! stop,
    ok.

-spec redraw_progress_bar(io_lib:chars(), io_lib:chars()) -> ok.
redraw_progress_bar(LastShownProgressBar, ProgressBar) ->
    Eraser = lists:duplicate(lists:flatlength(LastShownProgressBar), "\b"),
    draw_progress_bar([Eraser, ProgressBar]).

-spec erase_progress_bar(io_lib:chars()) -> ok.
erase_progress_bar(LastShownProgressBar) ->
    draw_progress_bar(lists:duplicate(lists:flatlength(LastShownProgressBar), "\b \b")).
