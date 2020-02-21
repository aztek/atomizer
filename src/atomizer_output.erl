-module(atomizer_output).

-export([
    start/0,
    put_chars/2,
    set_progress/1,
    hide_progress/0
]).

-define(PROCESS_NAME, ?MODULE).

-define(PROGRESS_BAR_DEVICE, standard_error).

-spec start() -> true.
start() ->
    register(?PROCESS_NAME, spawn(fun () -> loop() end)).

-spec loop() -> no_return().
loop() ->
    receive
        {output, IoDevice, Message} ->
            io:put_chars(IoDevice, Message),
            loop();

        {set_progress, ProgressBar} ->
            draw_progress_bar(ProgressBar),
            loop_progress(ProgressBar);

        hide_progress ->
            loop()
    end.

-spec loop_progress(string()) -> no_return().
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
            loop()
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

-spec draw_progress_bar(string()) -> ok.
draw_progress_bar(ProgressBar) ->
    io:put_chars(?PROGRESS_BAR_DEVICE, ProgressBar).

-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-spec redraw_progress_bar(string(), string()) -> ok.
redraw_progress_bar(LastShownProgressBar, ProgressBar) ->
    io:put_chars(?PROGRESS_BAR_DEVICE, ?HIDE_CURSOR),
    io:put_chars(?PROGRESS_BAR_DEVICE, lists:duplicate(length(LastShownProgressBar), "\b")),
    draw_progress_bar(ProgressBar),
    io:put_chars(?PROGRESS_BAR_DEVICE, ?SHOW_CURSOR).

-spec erase_progress_bar(string()) -> ok.
erase_progress_bar(LastShownProgressBar) ->
    io:put_chars(?PROGRESS_BAR_DEVICE, ?HIDE_CURSOR),
    io:put_chars(?PROGRESS_BAR_DEVICE, lists:duplicate(length(LastShownProgressBar), "\b \b")),
    io:put_chars(?PROGRESS_BAR_DEVICE, ?SHOW_CURSOR).
