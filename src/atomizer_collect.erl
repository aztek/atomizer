-module(atomizer_collect).

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 4).

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
        {ok, Sources} ->
            Collection = ets:new(collection, [private, set]),
            Pool = ets:new(pool, [private, set]),
            Queue = ets:new(queue, [private, ordered_set]),
            ets:insert(Queue, [{Source} || Source <- Sources]),
            Result = loop(Pid, Collection, Pool, Queue, Package),
            ets:delete(Queue),
            ets:delete(Pool),
            ets:delete(Collection),
            Result;
        {error, Error} -> {error, Error}
    end.

-spec collect_sources(atomizer:package()) -> {ok, [atomizer:source()]} | {error, term()}.
collect_sources(Package) ->
    Paths = atomizer:package_paths(Package),
    ParseBeams = atomizer:package_parse_beams(Package),
    Collect = fun (_, {error, Error}) -> {error, Error};
                  (Path, {ok, Sources}) ->
                      case atomizer_traverse:detect_source(Path, Package) of
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

-spec loop(pid(), ets:tid(), ets:tid(), ets:tid(), atomizer:package()) ->
    {ok, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, term()}.
loop(Pid, Collection, Pool, Queue, Package) ->
    case {ets:info(Pool, size), ets:info(Queue, size)} of
        {0, 0} ->
            {NrFiles, NrDirs} = collected_statistics(Collection),
            {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            Source = ets:first(Queue),
            ets:delete(Queue, Source),
            spawn_link(atomizer_traverse, traverse, [self(), Source, Package]),
            ets:insert(Pool, {Source}),
            loop(Pid, Collection, Pool, Queue, Package);

        _ ->
            receive
                {add_source, Source = {Type, _}} ->
                    SkipSource = ets:member(Pool, Source) orelse
                                 ets:member(Collection, Source) orelse
                                 ets:member(Queue, Source) orelse
                                 (not atomizer:package_parse_beams(Package) andalso (Type == beam)),
                    case SkipSource of
                        true -> ok;
                        false ->
                            ets:insert(Queue, {Source}),
                            case Source of
                                {dir, _} -> ok;
                                _ -> atomizer_progress:increase_total(1)
                            end
                    end,
                    loop(Pid, Collection, Pool, Queue, Package);

                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop(Pid, Collection, Pool, Queue, Package);

                {done_source, Source} ->
                    case Source of
                        {dir, _} -> ok;
                        _ -> atomizer_progress:increase_elapsed(1)
                    end,
                    ets:insert(Collection, {Source}),
                    ets:delete(Pool, Source),
                    loop(Pid, Collection, Pool, Queue, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec collected_statistics(ets:tid()) -> {NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()}.
collected_statistics(Collection) ->
    ets:foldl(fun ({Source}, {NrFiles, NrDirs}) ->
                   case Source of
                       {dir, _} -> {NrFiles, NrDirs + 1};
                       _        -> {NrFiles + 1, NrDirs}
                   end
               end, {0, 0}, Collection).

-spec format_error({module(), term()}) -> io_lib:chars().
format_error({Module, Error}) ->
    Module:format_error(Error).
