-module(atomizer_output).

-behavior(gen_server).

-export([
    start_link/0,
    print/1,
    nl/0,
    error/1,
    warning/1,
    set_banner/1,
    hide_banner/0,
    stop/1,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

-type exit_code() :: non_neg_integer().

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, ""}.

-spec put_chars(io:device(), io_lib:chars()) -> ok.
put_chars(IoDevice, Message) ->
    gen_server:cast(?MODULE, {output, IoDevice, Message}).

-spec set_banner(string()) -> ok.
set_banner(Banner) ->
    gen_server:cast(?MODULE, {set_banner, Banner}).

-spec hide_banner() -> ok.
hide_banner() ->
    gen_server:cast(?MODULE, hide_banner).

-spec stop(exit_code()) -> ok.
stop(ExitCode) ->
    gen_server:stop(?MODULE, ExitCode, _Timeout = infinity).


print(Message) ->
    put_chars(standard_io, [Message, "\n"]).

nl() ->
    put_chars(standard_io, "\n").

-spec error(atomizer:error()) -> ok.
error(Error) ->
    Message = [atomizer_color:bold("Error: ") | atomizer:format_error(Error)],
    put_chars(standard_error, [atomizer_color:red(Message), "\n"]).

-spec warning(atomizer:error()) -> ok.
warning(Warning) ->
    case atomizer_cli_options:get_verbosity() of
        0 -> ok;
        _ ->
            Message = [atomizer_color:bold("Warning: ") | atomizer:format_error(Warning)],
            put_chars(standard_error, [atomizer_color:yellow(Message), "\n"])
    end.


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({output, IoDevice, Message}, LastShownBanner) ->
    erase_banner(LastShownBanner),
    io:put_chars(IoDevice, Message),
    draw_banner(LastShownBanner),
    {noreply, LastShownBanner};

handle_cast({set_banner, LastShownBanner}, LastShownBanner) ->
    {noreply, LastShownBanner};

handle_cast({set_banner, Banner}, LastShownBanner) ->
    redraw_banner(LastShownBanner, Banner),
    {noreply, Banner};

handle_cast(hide_banner, LastShownBanner) ->
    erase_banner(LastShownBanner),
    {noreply, ""}.

-spec terminate(exit_code(), string()) -> no_return().
terminate(ExitCode, LastShownBanner) ->
    erase_banner(LastShownBanner),
    erlang:halt(ExitCode).


-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-spec draw_banner(io_lib:chars()) -> ok.
draw_banner([]) -> ok;
draw_banner(Banner) ->
    io:put_chars(standard_error, [?HIDE_CURSOR, Banner, ?SHOW_CURSOR]).

-spec redraw_banner(io_lib:chars(), io_lib:chars()) -> ok.
redraw_banner("", Banner) ->
    draw_banner(Banner);
redraw_banner(LastShownBanner, Banner) ->
    Eraser = lists:duplicate(lists:flatlength(LastShownBanner), $\b),
    draw_banner([Eraser | Banner]).

-spec erase_banner(io_lib:chars()) -> ok.
erase_banner(LastShownBanner) ->
    draw_banner(lists:duplicate(lists:flatlength(LastShownBanner), "\b \b")).
