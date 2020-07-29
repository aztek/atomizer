-module(atomizer_collect).

-include_lib("kernel/include/file.hrl").

-define(OPEN_DIR_LIMIT, 4).
-define(OPEN_FILE_LIMIT, 4).

-export([
    collect/2
]).

-spec collect(pid(), atomizer:package()) ->
    {done_atoms, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, atomizer:error()}.
collect(Pid, Package) ->
    case collect_paths(Pid, Package) of
        {ok, NrFiles, NrDirs} -> Pid ! {done_atoms, NrFiles, NrDirs};
        {error, Error} -> Pid ! {error, Error}
    end.

-spec collect_paths(pid(), atomizer:package()) ->
    {ok, NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()} | {error, atomizer:error()}.
collect_paths(Pid, Package) ->
    case collect_sources(Package) of
        {ok, Sources} ->
            Pool  = ets:new(pool,  [private, set]),
            Files = ets:new(files, [private, set]),
            Dirs  = ets:new(dirs,  [private, set]),
            lists:foreach(fun ({dir, Dir}) -> ets:insert(Dirs, {Dir});
                              (File) -> ets:insert(Files, {File})
                          end, Sources),
            Result = case loop_dirs(_NrDirs = 0, _NrFiles = 0, Pool, Dirs, Files, Package) of
                         {ok, NrDirs, NrFiles} ->
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

-spec loop_dirs(NrDirs, NrFiles, ets:tid(), ets:tid(), ets:tid(), atomizer:package()) ->
    {ok, NrDirs, NrFiles} | {error, atomizer:error()} when
    NrDirs  :: non_neg_integer(),
    NrFiles :: non_neg_integer().
loop_dirs(NrDirs, NrFiles, Pool, Dirs, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Dirs, size)} of
        {0, 0} -> {ok, NrFiles, NrDirs};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_DIR_LIMIT, QueueSize > 0 ->
            Dir = ets:first(Dirs),
            ets:delete(Dirs, Dir),
            Self = self(),
            spawn_link(fun () -> Self ! atomizer_traverse:traverse(Self, Package, Dir) end),
            ets:insert(Pool, {Dir}),
            loop_dirs(NrDirs, NrFiles, Pool, Dirs, Files, Package);

        _ ->
            receive
                {add_source, Source} ->
                    SkipSource = ets:member(Pool, Source) orelse
                                 ets:member(Dirs, Source) orelse
                                 ets:member(Files, Source),
                    case SkipSource of
                        true -> ok;
                        false ->
                            case Source of
                                {dir, Dir} -> ets:insert(Dirs, {Dir});
                                File -> ets:insert(Files, {File})
                            end
                    end,
                    loop_dirs(NrDirs, NrFiles, Pool, Dirs, Files, Package);

                {done_dir, Dir} ->
                    atomizer_progress:tick("Collecting files and directories (~p)"),
                    ets:delete(Pool, Dir),
                    loop_dirs(NrDirs + 1, NrFiles, Pool, Dirs, Files, Package);

                {done_file, File} ->
                    atomizer_progress:tick("Collecting files and directories (~p)"),
                    ets:delete(Pool, File),
                    loop_dirs(NrDirs, NrFiles + 1, Pool, Dirs, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec loop_files(pid(), ets:tid(), ets:tid(), atomizer:package()) -> ok | {error, atomizer:error()}.
loop_files(Pid, Pool, Files, Package) ->
    case {ets:info(Pool, size), ets:info(Files, size)} of
        {0, 0} -> ok;

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            File = ets:first(Files),
            ets:delete(Files, File),
            Self = self(),
            spawn_link(fun () -> Self ! atomizer_parse:parse(Self, Package, File) end),
            ets:insert(Pool, {File}),
            loop_files(Pid, Pool, Files, Package);

        _ ->
            receive
                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop_files(Pid, Pool, Files, Package);

                {done_file, File} ->
                    atomizer_progress:progress(1),
                    ets:delete(Pool, File),
                    loop_files(Pid, Pool, Files, Package);

                {error, Error} ->
                    {error, Error}
            end
    end.
