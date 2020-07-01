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

    bg_red/1,
    bg_yellow/1,
    bg_green/1,
    bg_blue/1,
    bg_magenta/1,
    bg_cyan/1,

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

-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-spec draw_progress_bar(io_lib:chars()) -> ok.
draw_progress_bar(ProgressBar) ->
    io:put_chars(?PROGRESS_BAR_DEVICE, [?HIDE_CURSOR, ProgressBar, ?SHOW_CURSOR]).

-spec halt(non_neg_integer()) -> ok.
halt(ExitCode) ->
    ?PROCESS_NAME ! {halt, ExitCode},
    ok.

-spec redraw_progress_bar(io_lib:chars(), io_lib:chars()) -> ok.
redraw_progress_bar(LastShownProgressBar, ProgressBar) ->
    Eraser = lists:duplicate(lists:flatlength(LastShownProgressBar), "\b"),
    draw_progress_bar([Eraser, ProgressBar]).

-spec erase_progress_bar(io_lib:chars()) -> ok.
erase_progress_bar(LastShownProgressBar) ->
    draw_progress_bar(lists:duplicate(length(LastShownProgressBar), "\b \b")).


%%% Colored ASCII output

-define(CLEAR,     "\e[00m").
-define(BOLD,      "\e[1m").
-define(ITALIC,    "\e[3m").
-define(INVERT,    "\e[7m").

-define(FG_RED,    "\e[31m").
-define(FG_GREEN,  "\e[32m").
-define(FG_YELLOW, "\e[33m").
-define(FG_BLUE,   "\e[34m").
-define(FG_MAGENTA,"\e[35m").
-define(FG_CYAN,   "\e[36m").

-define(BG_RED,    "\e[41m").
-define(BG_GREEN,  "\e[42m").
-define(BG_YELLOW, "\e[43m").
-define(BG_BLUE,   "\e[44m").
-define(BG_MAGENTA,"\e[45m").
-define(BG_CYAN,   "\e[46m").

-spec ascii_color(string(), io_lib:chars()) -> io_lib:chars().
ascii_color(Color, Chars) -> [Color, ascii_recolor(Color, Chars), ?CLEAR].

-spec ascii_recolor(string(), io_lib:chars()) -> io_lib:chars().
ascii_recolor(Color, Chars) ->
    lists:flatmap(fun (?CLEAR) -> [?CLEAR, Color];
                      (Chunks) when is_list(Chunks) -> [ascii_recolor(Color, Chunks)];
                      (Char) -> [Char]
                  end,
                  Chars).

-spec bold(io_lib:chars()) -> io_lib:chars().
bold(Chars) -> ascii_color(?BOLD, Chars).

-spec italic(io_lib:chars()) -> io_lib:chars().
italic(Chars) -> ascii_color(?ITALIC, Chars).

-spec invert(io_lib:chars()) -> io_lib:chars().
invert(Chars) -> ascii_color(?INVERT, Chars).

-spec red(io_lib:chars()) -> io_lib:chars().
red(Chars) -> ascii_color(?FG_RED, Chars).

-spec yellow(io_lib:chars()) -> io_lib:chars().
yellow(Chars) -> ascii_color(?FG_YELLOW, Chars).

-spec blue(io_lib:chars()) -> io_lib:chars().
blue(Chars) -> ascii_color(?FG_BLUE, Chars).

-spec green(io_lib:chars()) -> io_lib:chars().
green(Chars) -> ascii_color(?FG_GREEN, Chars).

-spec magenta(io_lib:chars()) -> io_lib:chars().
magenta(Chars) -> ascii_color(?FG_MAGENTA, Chars).

-spec cyan(io_lib:chars()) -> io_lib:chars().
cyan(Chars) -> ascii_color(?FG_CYAN, Chars).


-spec bg_red(bg_io_lib:chars()) -> io_lib:chars().
bg_red(Chars) -> ascii_color(?BG_RED, Chars).

-spec bg_yellow(io_lib:chars()) -> io_lib:chars().
bg_yellow(Chars) -> ascii_color(?BG_YELLOW, Chars).

-spec bg_blue(io_lib:chars()) -> io_lib:chars().
bg_blue(Chars) -> ascii_color(?BG_BLUE, Chars).

-spec bg_green(io_lib:chars()) -> io_lib:chars().
bg_green(Chars) -> ascii_color(?BG_GREEN, Chars).

-spec bg_magenta(io_lib:chars()) -> io_lib:chars().
bg_magenta(Chars) -> ascii_color(?BG_MAGENTA, Chars).

-spec bg_cyan(io_lib:chars()) -> io_lib:chars().
bg_cyan(Chars) -> ascii_color(?BG_CYAN, Chars).
