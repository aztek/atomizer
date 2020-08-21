-module(atomizer_collect).

-include_lib("kernel/include/file.hrl").

-define(OPEN_DIR_LIMIT, 16).
-define(OPEN_FILE_LIMIT, 16).

-export([
    start/1,
    add_source/1
]).

-define(PROCESS_NAME, ?MODULE).

-spec start(atomizer:package()) -> true.
start(Package) ->
    register(?PROCESS_NAME,
             spawn(fun () ->
                       case collect(Package) of
                           {ok, NrFiles, NrDirs} -> atomizer_sup:done_atoms(NrFiles, NrDirs);
                           {error, Error} -> atomizer_sup:fail(Error)
                       end
                   end)).

-spec add_source(atomizer:source()) -> ok.
add_source(Source) ->
    ?PROCESS_NAME ! {add_source, Source},
    ok.

-spec collect(atomizer:package()) ->
    {ok, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, atomizer:error()}.
collect(Package) ->
    case collect_sources(Package) of
        {ok, Sources} ->
            Pool  = ets:new(pool,  [private, set]),
            Files = ets:new(files, [private, set]),
            Dirs  = ets:new(dirs,  [private, set]),
            lists:foreach(fun ({dir, Dir}) -> ets:insert(Dirs, {Dir});
                              (File) -> ets:insert(Files, {File})
                          end, Sources),
            Result = case loop_dirs(_NrFiles = 0, _NrDirs = 0, Pool, Dirs, Files, Package) of
                         {ok, NrFiles, NrDirs} ->
                             atomizer_progress:next(_Elapsed = 0, NrFiles),
                             case loop_files(Pool, Files, Package) of
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

-spec collect_sources(atomizer:package()) -> {ok, [atomizer:source()]} | {error, atomizer:error()}.
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
                                      atomizer:warning(Error),
                                      {ok, Sources};
                                  false ->
                                      {error, Error}
                              end
                      end
              end,
    lists:foldl(Collect, {ok, []}, atomizer:package_paths(Package)).

-spec loop_dirs(NrFiles, NrDirs, ets:tid(), ets:tid(), ets:tid(), atomizer:package()) ->
    {ok, NrFiles, NrDirs} | {error, atomizer:error()} when
    NrDirs  :: non_neg_integer(),
    NrFiles :: non_neg_integer().
loop_dirs(NrFiles, NrDirs, Pool, Dirs, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Dirs, size)} of
        {0, 0} -> {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_DIR_LIMIT, QueueSize > 0 ->
            Dir = ets:first(Dirs),
            ets:delete(Dirs, Dir),
            spawn_link(fun () ->
                           case atomizer_traverse:traverse(Package, Dir) of
                               ok    -> ?PROCESS_NAME ! {done_dir, Dir};
                               Error -> ?PROCESS_NAME ! Error
                           end
                       end),
            ets:insert(Pool, {Dir}),
            loop_dirs(NrFiles, NrDirs, Pool, Dirs, Files, Package);

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
                            case Source of
                                {dir, Dir} ->
                                    ets:insert(Dirs, {Dir}),
                                    loop_dirs(NrFiles, NrDirs + 1, Pool, Dirs, Files, Package);

                                File ->
                                    ets:insert(Files, {File}),
                                    loop_dirs(NrFiles + 1, NrDirs, Pool, Dirs, Files, Package)
                            end
                    end;

                {done_dir, Dir} ->
                    ets:delete(Pool, Dir),
                    loop_dirs(NrFiles, NrDirs, Pool, Dirs, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec loop_files(ets:tid(), ets:tid(), atomizer:package()) -> ok | {error, atomizer:error()}.
loop_files(Pool, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Files, size)} of
        {0, 0} -> ok;

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            File = ets:first(Files),
            ets:delete(Files, File),
            spawn_link(fun () ->
                           case atomizer_parse:parse(Package, File) of
                               ok    -> ?PROCESS_NAME ! {done_file, File};
                               Error -> ?PROCESS_NAME ! Error
                           end
                       end),
            ets:insert(Pool, {File}),
            loop_files(Pool, Files, Package);

        _ ->
            receive
                {done_file, File} ->
                    atomizer_progress:progress(1),
                    ets:delete(Pool, File),
                    loop_files(Pool, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.
