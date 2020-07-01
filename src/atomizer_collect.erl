-module(atomizer_collect).

-include_lib("kernel/include/file.hrl").

-define(OPEN_DIR_LIMIT, 4).
-define(OPEN_FILE_LIMIT, 4).

-export([
    collect/2,
    format_error/1
]).

-spec collect(pid(), atomizer:package()) ->
    {done_atoms, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, {?MODULE, term()}}.
collect(Pid, Package) ->
    case collect_paths(Pid, Package) of
        {ok, NrFiles, NrDirs} -> Pid ! {done_atoms, NrFiles, NrDirs};
        {error, Error} -> Pid ! {error, {?MODULE, Error}}
    end.

-spec collect_paths(pid(), atomizer:package()) ->
    {ok, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, term()}.
collect_paths(Pid, Package) ->
    case collect_sources(Package) of
        {ok, Sources} ->
            Pool  = ets:new(pool,  [private, set]),
            Files = ets:new(files, [private, set]),
            Dirs  = ets:new(dirs,  [private, set]),
            lists:foreach(fun (Source) ->
                              Queue = case Source of {dir, _} -> Dirs; _ -> Files end,
                              ets:insert(Queue, {Source})
                          end, Sources),
            Result = case loop_dirs(_NrDirs = 0, Pool, Dirs, Files, Package) of
                         {ok, NrDirs} ->
                             NrFiles = ets:info(Files, size),
                             atomizer_progress:next(NrFiles),
                             case loop_files(Pid, Pool, Files, Package) of
                                 ok -> {ok, NrFiles, NrDirs};
                                 {error, Error} -> {error, Error}
                             end;
                         {error, Error} -> {error, Error}
                     end,
            ets:delete(Dirs),
            ets:delete(Files),
            ets:delete(Pool),
            Result;
        {error, Error} -> {error, Error}
    end.

-spec collect_sources(atomizer:package()) -> {ok, [atomizer:source()]} | {error, term()}.
collect_sources(Package) ->
    Collect = fun (_, {error, Error}) -> {error, Error};
                  (Path, {ok, Sources}) ->
                      case atomizer_traverse:detect_source(Path, Package) of
                          {ok, Source} ->
                              atomizer_progress:tick("Collecting files and directories (~p)"),
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
    lists:foldl(Collect, {ok, []}, atomizer:package_paths(Package)).

-spec loop_dirs(non_neg_integer(), ets:tid(), ets:tid(), ets:tid(), atomizer:package()) ->
    {ok, NrDirs :: non_neg_integer()} | {error, term()}.
loop_dirs(NrDirs, Pool, Dirs, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Dirs, size)} of
        {0, 0} -> {ok, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_DIR_LIMIT, QueueSize > 0 ->
            Source = ets:first(Dirs),
            ets:delete(Dirs, Source),
            spawn_link(atomizer_traverse, traverse, [self(), Source, Package]),
            ets:insert(Pool, {Source}),
            loop_dirs(NrDirs, Pool, Dirs, Files, Package);

        _ ->
            receive
                {add_source, Source} ->
                    SkipSource = ets:member(Pool, Source) orelse
                                 ets:member(Dirs, Source) orelse
                                 ets:member(Files, Source),
                    case SkipSource of
                        true -> ok;
                        false ->
                            atomizer_progress:tick("Collecting files and directories (~p)"),
                            Queue = case Source of {dir, _} -> Dirs; _ -> Files end,
                            ets:insert(Queue, {Source})
                    end,
                    loop_dirs(NrDirs, Pool, Dirs, Files, Package);

                {done_source, Source} ->
                    ets:delete(Pool, Source),
                    loop_dirs(NrDirs + 1, Pool, Dirs, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec loop_files(pid(), ets:tid(), ets:tid(), atomizer:package()) ->
    {ok, NrFiles :: non_neg_integer()} | {error, term()}.
loop_files(Pid, Pool, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Files, size)} of
        {0, 0} -> ok;

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            Source = ets:first(Files),
            ets:delete(Files, Source),
            spawn_link(atomizer_traverse, traverse, [self(), Source, Package]),
            ets:insert(Pool, {Source}),
            loop_files(Pid, Pool, Files, Package);

        _ ->
            receive
                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop_files(Pid, Pool, Files, Package);

                {done_source, Source} ->
                    atomizer_progress:progress(1),
                    ets:delete(Pool, Source),
                    loop_files(Pid, Pool, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec format_error({module(), term()}) -> io_lib:chars().
format_error({Module, Error}) ->
    Module:format_error(Error).
