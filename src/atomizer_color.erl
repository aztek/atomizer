-module(atomizer_color).

-export([
    red/1,
    yellow/1,
    green/1,
    blue/1,
    magenta/1,
    cyan/1,
    grey/1,

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
-define(FG_GREY,   "\e[90m").

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

-spec grey(io_lib:chars()) -> io_lib:chars().
grey(Chars) -> ascii_color(?FG_GREY, Chars).


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
