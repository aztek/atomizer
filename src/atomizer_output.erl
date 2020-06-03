-module(atomizer_output).

-export([
    start/1,
    put_chars/2,
    set_progress/1,
    hide_progress/0,
    halt/1,

    red/1,
    yellow/1,
    green/1,
    blue/1,
    magenta/1,
    cyan/1,
    bold/1,
    italic/1,
    invert/1
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


%%% Colored ASCII output

-define(RED,    "\e[31m").
-define(GREEN,  "\e[32m").
-define(YELLOW, "\e[33m").
-define(BLUE,   "\e[34m").
-define(MAGENTA,"\e[35m").
-define(CYAN,   "\e[36m").
-define(BOLD,   "\e[1m").
-define(ITALIC, "\e[3m").
-define(INVERT, "\e[7m").
-define(CLEAR,  "\e[00m").

-spec ascii_color(string(), io_lib:chars()) -> io_lib:chars().
ascii_color(Color, Chars) -> [Color, ascii_recolor(Color, Chars), ?CLEAR].

-spec ascii_recolor(string(), io_lib:chars()) -> io_lib:chars().
ascii_recolor(Color, Chars) ->
    lists:flatmap(fun (?CLEAR) -> [?CLEAR, Color];
                      (Chunks) when is_list(Chunks) -> [ascii_recolor(Color, Chunks)];
                      (Char) -> [Char] end,
                  Chars).

-spec red(io_lib:chars()) -> io_lib:chars().
red(Chars) -> ascii_color(?RED, Chars).

-spec yellow(io_lib:chars()) -> io_lib:chars().
yellow(Chars) -> ascii_color(?YELLOW, Chars).

-spec blue(io_lib:chars()) -> io_lib:chars().
blue(Chars) -> ascii_color(?BLUE, Chars).

-spec green(io_lib:chars()) -> io_lib:chars().
green(Chars) -> ascii_color(?GREEN, Chars).

-spec magenta(io_lib:chars()) -> io_lib:chars().
magenta(Chars) -> ascii_color(?MAGENTA, Chars).

-spec cyan(io_lib:chars()) -> io_lib:chars().
cyan(Chars) -> ascii_color(?CYAN, Chars).

-spec bold(io_lib:chars()) -> io_lib:chars().
bold(Chars) -> ascii_color(?BOLD, Chars).

-spec italic(io_lib:chars()) -> io_lib:chars().
italic(Chars) -> ascii_color(?ITALIC, Chars).

-spec invert(io_lib:chars()) -> io_lib:chars().
invert(Chars) -> ascii_color(?INVERT, Chars).
