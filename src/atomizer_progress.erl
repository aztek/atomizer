-module(atomizer_progress).

-behavior(gen_server).

-export([
    start_link/2,
    progress/1,
    hide/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(PROGRESS_BAR_SCALE, 70).
-define(PROGRESS_BAR_WIDTH, ?PROGRESS_BAR_SCALE + 7).

-type progress() :: 0..100.

-record(state, {
    total   :: non_neg_integer(),
    elapsed :: non_neg_integer(),
    last_shown_progress = 0 :: progress()
}).

-spec start_link(non_neg_integer(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(Elapsed, Total) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Elapsed, Total], []).

init([Elapsed, Total]) ->
    {ok, #state{total = Total, elapsed = Elapsed}}.

-spec progress(non_neg_integer()) -> ok.
progress(Delta) ->
    gen_server:cast(?MODULE, {progress, Delta}).

-spec hide() -> ok.
hide() ->
    gen_server:cast(?MODULE, hide).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({progress, Delta}, State) ->
    #state{total = Total, elapsed = Elapsed, last_shown_progress = LastShownProgress} = State,
    IncreasedElapsed = Elapsed + Delta,
    IncreasedProgress = case Total of
                            0 -> 100;
                            _ -> erlang:floor(100 * IncreasedElapsed / Total)
                        end,
    Progress = erlang:min(100, IncreasedProgress),
    case Progress > LastShownProgress of
        true ->
            atomizer_output:set_banner(render_progress_bar(Progress)),
            {noreply, State#state{elapsed = IncreasedElapsed, last_shown_progress = Progress}};

        false ->
            {noreply, State#state{elapsed = IncreasedElapsed}}
    end;

handle_cast(hide, State) ->
    atomizer_output:hide_banner(),
    {noreply, State#state{last_shown_progress = 100}}.

-spec render_progress_bar(progress()) -> io_lib:chars().
render_progress_bar(Progress) ->
    Elapsed = erlang:ceil(?PROGRESS_BAR_SCALE * Progress / 100),
    Bar = lists:duplicate(?PROGRESS_BAR_SCALE - Elapsed, $ ),
    Format = lists:flatten(io_lib:format("[~~~p..#s]~~3.w% ", [?PROGRESS_BAR_SCALE])),
    io_lib:format(Format, [Bar, Progress]).
