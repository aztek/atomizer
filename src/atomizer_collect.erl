-module(atomizer_collect).

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([
    collect/2,
    format_error/1
]).

-spec collect(pid(), atomizer:package()) -> {ok, non_neg_integer(), non_neg_integer()} | {error, {?MODULE, term()}}.
collect(Pid, Package) ->
    case collect_paths(Pid, Package) of
        {ok, NrFiles, NrDirs} -> Pid ! {done_atoms, NrFiles, NrDirs};
        {error, Error} -> Pid ! {error, {?MODULE, Error}}
    end.

-spec collect_paths(pid(), atomizer:package()) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
collect_paths(Pid, Package) ->
    case collect_sources(Package) of
        {ok, Sources}  -> loop(Pid, sets:new(), sets:new(), queue:from_list(Sources), Package);
        {error, Error} -> {error, Error}
    end.

-spec collect_sources(atomizer:package()) -> {ok, [atomizer:source()]} | {error, term()}.
collect_sources(Package) ->
    Paths = atomizer:package_paths(Package),
    ParseBeams = atomizer:package_parse_beams(Package),
    Collect = fun (_, {error, Error}) -> {error, Error};
                  (Path, {ok, Sources}) ->
                      case atomizer_traverse:detect_source(Path) of
                          {ok, {beam, _}} when not ParseBeams -> {ok, Sources};
                          {ok, Source} ->
                              case Source of
                                  {dir, _} -> ok;
                                  _ -> atomizer_progress:increase_total(1)
                              end,
                              {ok, [Source | Sources]};
                          ignore -> {ok, Sources};
                          {error, Error} ->
                              case atomizer_cli_options:get_warn_errors() of
                                  true ->
                                      atomizer:warning(format_error(Error)),
                                      {ok, Sources};
                                  false ->
                                      {error, Error}
                              end
                      end
              end,
    lists:foldl(Collect, {ok, []}, Paths).

-spec loop(pid(), Collection, Pool, Queue, Package) -> {ok, NrFiles, NrDirs} | {error, term()} when
    NrFiles      :: non_neg_integer(),
    NrDirs       :: non_neg_integer(),
    Collection   :: sets:set(atomizer:source()),
    Pool         :: sets:set(atomizer:source()),
    Queue        :: queue:queue(atomizer:source()),
    Package      :: atomizer:package().
loop(Pid, Collection, Pool, Queue, Package) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            {NrFiles, NrDirs} = collected_statistics(Collection),
            {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Source}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_traverse, traverse, [self(), Source, atomizer:package_includes(Package)]),
            loop(Pid, Collection, sets:add_element(Source, Pool), TailQueue, Package);

        _ ->
            receive
                {add_source, Source = {Type, _}} ->
                    case Source of
                        {dir, _} -> ok;
                        _ -> atomizer_progress:increase_total(1)
                    end,
                    SkipSource = sets:is_element(Source, Pool) orelse
                                 sets:is_element(Source, Collection) orelse
                                 (not atomizer:package_parse_beams(Package) andalso (Type == beam)),
                    UpdatedQueue = case SkipSource of true -> Queue; false -> queue:in(Source, Queue) end,
                    loop(Pid, Collection, Pool, UpdatedQueue, Package);

                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop(Pid, Collection, Pool, Queue, Package);

                {done_source, Source} ->
                    case Source of
                        {dir, _} -> ok;
                        _ -> atomizer_progress:increase_elapsed(1)
                    end,
                    loop(Pid, sets:add_element(Source, Collection), sets:del_element(Source, Pool), Queue, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec collected_statistics(sets:set(atomizer:source())) -> {NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()}.
collected_statistics(Collection) ->
    sets:fold(fun (Source, {NrFiles, NrDirs}) ->
                  case Source of
                      {dir, _} -> {NrFiles, NrDirs + 1};
                      _        -> {NrFiles + 1, NrDirs}
                  end
              end, {0, 0}, Collection).

-spec format_error({module(), term()}) -> io_lib:chars().
format_error({Module, Error}) ->
    Module:format_error(Error).
