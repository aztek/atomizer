-module(atomizer_collect).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([
    collect/3,
    format_error/1
]).

-spec collect(pid(), [file:filename()], [file:filename()]) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
collect(Pid, Paths, IncludePaths) ->
    case collect_paths(Pid, Paths, IncludePaths) of
        {ok, NrFiles, NrDirs} -> Pid ! {done_atoms, NrFiles, NrDirs};
        {error, Error} -> Pid ! {error, {?MODULE, Error}}
    end.

-spec collect_paths(pid(), [file:filename()], [file:filename()]) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
collect_paths(Pid, Paths, IncludePaths) ->
    case collect_sources(Paths) of
        {ok, Sources}  -> loop(Pid, sets:new(), sets:new(), queue:from_list(Sources), IncludePaths);
        {error, Error} -> {error, Error}
    end.

-spec collect_sources([file:filename()]) -> {ok, [source()]} | {error, term()}.
collect_sources(Paths) ->
    lists:foldl(fun (_, {error, Error}) -> {error, Error};
                    (Path, {ok, Sources}) ->
                        case atomizer_traverse:detect_source(Path) of
                            {ok, other}    -> {ok, Sources};
                            {ok, Source}   -> {ok, [Source | Sources]};
                            {error, Error} -> {error, Error}
                        end
                end,
                {ok, []}, Paths).

-spec loop(pid(), Collection, Pool, Queue, IncludePaths) -> {ok, NrFiles, NrDirs} | {error, term()} when
    NrFiles      :: non_neg_integer(),
    NrDirs       :: non_neg_integer(),
    Collection   :: sets:set(source()),
    Pool         :: sets:set(source()),
    Queue        :: queue:queue(source()),
    IncludePaths :: [file:filename()].
loop(Pid, Collection, Pool, Queue, IncludePaths) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            {NrFiles, NrDirs} = sets:fold(fun (Source, {NrFiles, NrDirs}) ->
                                              case Source of
                                                  {erl, _} -> {NrFiles + 1, NrDirs};
                                                  {dir, _} -> {NrFiles, NrDirs + 1};
                                                  _        -> {NrFiles, NrDirs}
                                              end
                                          end, {0, 0}, Collection),
            {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Source}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_traverse, traverse, [self(), Source, IncludePaths]),
            loop(Pid, Collection, sets:add_element(Source, Pool), TailQueue, IncludePaths);

        _ ->
            receive
                {add_source, Source} ->
                    UpdatedQueue = case sets:is_element(Source, Pool) orelse sets:is_element(Source, Collection) of
                        true  -> Queue;
                        false -> queue:in(Source, Queue)
                    end,
                    loop(Pid, Collection, Pool, UpdatedQueue, IncludePaths);

                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop(Pid, Collection, Pool, Queue, IncludePaths);

                {done_source, Source} ->
                    loop(Pid, sets:add_element(Source, Collection), sets:del_element(Source, Pool), Queue, IncludePaths);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec format_error({module(), term()}) -> string().
format_error({Module, Error}) ->
    Module:format_error(Error).
