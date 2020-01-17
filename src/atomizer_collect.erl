-module(atomizer_collect).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([
    collect/2,
    format_error/1
]).

-spec collect(pid(), [file:filename()]) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
collect(Pid, Paths) ->
    case collect_paths(Pid, Paths) of
        {ok, NrFiles, NrDirs} -> Pid ! {done_atoms, NrFiles, NrDirs};
        {error, Error} -> Pid ! {error, {?MODULE, Error}}
    end.

-spec collect_paths(pid(), [file:filename()]) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
collect_paths(Pid, Paths) ->
    case collect_sources(Paths) of
        {ok, Sources}  -> loop(Pid, {0, 0}, sets:new(), queue:from_list(Sources));
        {error, Error} -> {error, Error}
    end.

-spec collect_sources([file:filename()]) -> {ok, [source()]} | {error, term()}.
collect_sources(Paths) ->
    lists:foldl(fun (_, {error, Error}) -> {error, Error};
                    (Path, {ok, Sources}) ->
                        case atomizer_parse:detect_source(Path) of
                            {ok, other}    -> {ok, Sources};
                            {ok, Source}   -> {ok, [Source | Sources]};
                            {error, Error} -> {error, Error}
                        end
                end,
                {ok, []}, Paths).

-spec loop(pid(), {NrFiles, NrDirs}, Pool, Queue) -> {ok, NrFiles, NrDirs} | {error, term()} when
    NrFiles :: non_neg_integer(),
    NrDirs  :: non_neg_integer(),
    Pool    :: sets:set(source()),
    Queue   :: queue:queue(source()).
loop(Pid, NrParsed, Pool, Queue) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            {NrFiles, NrDirs} = NrParsed,
            {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Source}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_parse, parse, [self(), Source]),
            loop(Pid, NrParsed, sets:add_element(Source, Pool), TailQueue);

        _ ->
            receive
                {add_source, Source} ->
                    loop(Pid, NrParsed, Pool, queue:in(Source, Queue));

                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop(Pid, NrParsed, Pool, Queue);

                {done_source, Source} ->
                    {NrFiles, NrDirs} = NrParsed,
                    IncrementedNrParsed = case Source of
                        {erl, _} -> {NrFiles + 1, NrDirs};
                        {dir, _} -> {NrFiles, NrDirs + 1};
                        _        -> {NrFiles, NrDirs}
                    end,
                    loop(Pid, IncrementedNrParsed, sets:del_element(Source, Pool), Queue);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec format_error({module(), term()}) -> string().
format_error({Module, Error}) ->
    Module:format_error(Error).
