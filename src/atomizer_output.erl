-module(atomizer_output).

-export([
    start/1,
    put_chars/2,
    set_progress/1,
    hide_progress/0,
    halt/1
]).

-define(PROCESS_NAME, ?MODULE).

-define(PROGRESS_BAR_DEVICE, standard_error).

-spec start(pid()) -> true.
start(Parent) ->
    register(?PROCESS_NAME, spawn(fun () -> Parent ! loop() end)).

-spec loop() -> {halt, ExitCode :: integer()}.
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

        {halt, ExitCode} ->
            {halt, ExitCode}
    end.

-spec loop_progress(string()) -> {halt, ExitCode :: integer()}.
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

        {halt, ExitCode} ->
            erase_progress_bar(LastShownProgressBar),
            {halt, ExitCode}
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

-spec halt(non_neg_integer()) -> ok.
halt(ExitCode) ->
    ?PROCESS_NAME ! {halt, ExitCode},
    ok.

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
