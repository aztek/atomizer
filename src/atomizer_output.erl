-module(atomizer_output).

-behavior(gen_server).

-export([
    start_link/0,
    put_chars/2,
    set_progress/1,
    hide_progress/0,
    stop/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

init(_Args) ->
    {ok, ""}.

handle_call(stop, _From, State) ->
    {reply, ok, State}.

handle_cast({output, IoDevice, Message}, LastShownProgressBar) ->
    erase_progress_bar(LastShownProgressBar),
    io:put_chars(IoDevice, Message),
    draw_progress_bar(LastShownProgressBar),
    {noreply, LastShownProgressBar};

handle_cast({set_progress, ProgressBar}, LastShownProgressBar) when ProgressBar == LastShownProgressBar ->
    {noreply, ProgressBar};

handle_cast({set_progress, ProgressBar}, LastShownProgressBar) ->
    redraw_progress_bar(LastShownProgressBar, ProgressBar),
    {noreply, ProgressBar};

handle_cast(hide_progress, LastShownProgressBar) ->
    erase_progress_bar(LastShownProgressBar),
    {noreply, ""}.

terminate(_Reason, LastShownProgressBar) ->
    erase_progress_bar(LastShownProgressBar).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec put_chars(io:device(), io_lib:chars()) -> ok.
put_chars(IoDevice, Message) ->
    gen_server:cast(?MODULE, {output, IoDevice, Message}).

-spec set_progress(string()) -> ok.
set_progress(ProgressBar) ->
    gen_server:cast(?MODULE, {set_progress, ProgressBar}).

-spec hide_progress() -> ok.
hide_progress() ->
    gen_server:cast(?MODULE, hide_progress).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).


-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-spec draw_progress_bar(io_lib:chars()) -> ok.
draw_progress_bar([]) -> ok;
draw_progress_bar(ProgressBar) ->
    io:put_chars(standard_error, [?HIDE_CURSOR, ProgressBar, ?SHOW_CURSOR]).

-spec redraw_progress_bar(io_lib:chars(), io_lib:chars()) -> ok.
redraw_progress_bar("", ProgressBar) ->
    draw_progress_bar(ProgressBar);
redraw_progress_bar(LastShownProgressBar, ProgressBar) ->
    Eraser = lists:duplicate(lists:flatlength(LastShownProgressBar), $\b),
    draw_progress_bar([Eraser | ProgressBar]).

-spec erase_progress_bar(io_lib:chars()) -> ok.
erase_progress_bar(LastShownProgressBar) ->
    draw_progress_bar(lists:duplicate(lists:flatlength(LastShownProgressBar), "\b \b")).
