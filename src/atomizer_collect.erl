-module(atomizer_collect).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([
    collect/2,
    format_error/1
]).

-spec collect(pid(), [file:filename()]) -> {ok, non_neg_integer()} | {error, term()}.
collect(Pid, Paths) ->
    case collect_paths(Pid, Paths) of
        {ok, NrParsed} -> Pid ! {done_atoms, NrParsed};
        {error, Error} -> Pid ! {error, {?MODULE, Error}}
    end.

-spec collect_paths(pid(), [file:filename()]) -> {ok, non_neg_integer()} | {error, term()}.
collect_paths(Pid, Paths) ->
    case collect_sources(Paths) of
        {ok, Sources}  -> loop(Pid, 0, sets:new(), queue:from_list(Sources));
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

-spec loop(pid(), non_neg_integer(), sets:set(source()), queue:queue(source())) -> {ok, non_neg_integer()} | {error, term()}.
loop(Pid, NrParsed, Pool, Queue) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            {ok, NrParsed};

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
                    loop(Pid, NrParsed + 1, sets:del_element(Source, Pool), Queue);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec format_error({module(), term()}) -> string().
format_error({Module, Error}) ->
    Module:format_error(Error).
