-module(atomizer_traverse).

-include_lib("kernel/include/file.hrl").

-define(OPEN_DIR_LIMIT, 16).
-define(MAX_LEVEL_SYMLINKS, 10).
-define(ERLANG_EXTENSIONS, [".erl", ".hrl"]).
-define(BEAM_EXTENSIONS, [".beam"]).

-export([
    start_link/2,
    detect_sources/2,
    format_error/1
]).

-export_type([
    error/0
]).

-type symlink() :: {Source :: file:filename(), Destination :: file:filename()}.
-type error()   :: {file:filename() | symlink(), atomizer:error()}.

-define(PROCESS_NAME, ?MODULE).

-record(state, {
    package :: atomizer:package(),
    pool = sets:new() :: sets:set(file:filename()),
    queue :: [file:filename()]
}).

-spec start_link([file:filename()], atomizer:package()) -> {ok, pid()}.
start_link(Dirs, Package) ->
    Pid = spawn_link(fun () ->
                         atomizer_spinner:start_link("Collecting files and directories (~p)"),
                         loop(#state{package = Package, queue = Dirs})
                     end),
    register(?PROCESS_NAME, Pid),
    {ok, Pid}.

-spec add_dir(file:filename()) -> ok.
add_dir(Dir) ->
    ?PROCESS_NAME ! {add_dir, Dir},
    ok.

-spec done_dir(file:filename()) -> ok.
done_dir(Dir) ->
    ?PROCESS_NAME ! {done_dir, Dir},
    ok.

-spec loop(#state{}) -> ok.
loop(#state{pool = Pool, queue = Queue} = State) ->
    case {sets:size(Pool), Queue} of
        {0, []} -> atomizer_spinner:hide();

        {NrTakenDescriptors, [Dir | RestQueue]} when NrTakenDescriptors < ?OPEN_DIR_LIMIT ->
            spawn_link(fun () ->
                           case traverse(State#state.package, Dir) of
                               ok -> done_dir(Dir);
                               {error, Error} -> erlang:exit({error, Error})
                           end
                       end),
            loop(State#state{pool = sets:add_element(Dir, Pool), queue = RestQueue});

        _ ->
            receive
                {add_dir, Dir} ->
                    case sets:is_element(Dir, Pool) of
                        true -> loop(State);
                        false -> loop(State#state{queue = [Dir | Queue]})
                    end;

                {done_dir, Dir} ->
                    atomizer_sup:done_dir(Dir),
                    atomizer_spinner:tick(),
                    loop(State#state{pool = sets:del_element(Dir, Pool)})
            end
    end.

-spec traverse(atomizer:package(), file:filename()) -> ok | {error, atomizer:error()}.
traverse(Package, Dir) ->
    case list_dir(Dir) of
        {ok, Paths} ->
            case detect_sources(Paths, Package) of
                {ok, Files, Dirs} ->
                    lists:foreach(fun atomizer_sup:file/1, Files),
                    lists:foreach(fun add_dir/1, Dirs);
                {error, Error} -> {error, Error}
            end;

        {error, Error} -> {error, Error}
    end.

-spec list_dir(file:filename()) -> {ok, [file:filename()]} | {error, atomizer:error()}.
list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Paths} ->
            {ok, [filename:join(Dir, Path) || Path <- Paths]};

        {error, Error} ->
            ExtendedError = {?MODULE, {Dir, {file, Error}}},
            case atomizer_cli_options:get_warn_errors() of
                true  -> atomizer:warning(ExtendedError), {ok, []};
                false -> {error, ExtendedError}
            end
    end.

-spec detect_sources(Paths :: [file:filename()], atomizer:package()) ->
    {ok, Files :: [atomizer:file()], Dirs :: [file:filename()]} | {error, atomizer:error()}.
detect_sources(Paths, Package) ->
    Collect = fun (_, {error, Error}) -> {error, Error};
                  (Path, {ok, Files, Dirs}) ->
                      case detect_source(Path, Package) of
                          {ok, {dir, Dir}} -> {ok, Files, [Dir | Dirs]};
                          {ok, File} -> {ok, [File | Files], Dirs};
                          ignore -> {ok, Files, Dirs};
                          {error, Error} ->
                              case atomizer_cli_options:get_warn_errors() of
                                  true ->
                                      atomizer:warning(Error),
                                      {ok, Files, Dirs};
                                  false ->
                                      {error, Error}
                              end
                      end
              end,
    lists:foldl(Collect, {ok, [], []}, Paths).

-spec detect_source(file:filename(), atomizer:package()) ->
    {ok, atomizer:source()} | ignore | {error, {?MODULE, error()}}.
detect_source(Path, Package) ->
    case resolve_real_path(Path, Package) of
        {ok, RealPath} ->
            Basename    = filename:basename(RealPath),
            IgnorePaths = atomizer:global_ignores() ++ atomizer:package_ignores(Package),
            case lists:member(Basename, IgnorePaths) of
                true  -> ignore;
                false ->
                    case file:read_file_info(RealPath) of
                        {ok, Info} ->
                            Type       = Info#file_info.type,
                            IsErlang   = is_erlang(RealPath),
                            IsBeam     = is_beam(RealPath),
                            ParseBeams = atomizer:package_parse_beams(Package),
                            case Type of
                                directory -> {ok, {dir, RealPath}};
                                regular when IsErlang -> {ok, {erl, RealPath}};
                                regular when IsBeam, ParseBeams -> {ok, {beam, RealPath}};
                                _ -> ignore
                            end;

                        {error, Error} ->
                            ExtendedError = {{Path, RealPath}, {file, Error}},
                            {error, {?MODULE, ExtendedError}}
                    end
            end;

        ignore -> ignore;

        {error, Error} ->
            {error, {?MODULE, Error}}
    end.

-spec resolve_real_path(file:filename(), atomizer:package()) ->
    {ok, file:filename()} | ignore | {error, error()}.
resolve_real_path(Path, Package) -> resolve_real_path(Path, Package, ?MAX_LEVEL_SYMLINKS).

-spec resolve_real_path(file:filename() | symlink(), atomizer:package(), Fuel :: non_neg_integer()) ->
    {ok, file:filename()} | ignore | {error, error()}.
resolve_real_path(Path, _Package, 0) -> {error, {Path, {file, eloop}}};
resolve_real_path(Path, Package, Fuel) ->
    FollowSymlinks = atomizer:package_follow_symlinks(Package),
    {Source, Destination} = case Path of {_, _} -> Path; _ -> {Path, Path} end,
    case file:read_link_all(Destination) of
        {ok, _} when not FollowSymlinks -> ignore;

        {ok, SymlinkPath} ->
            AbsoluteSymlinkPath =
                case filename:pathtype(SymlinkPath) of
                   absolute -> SymlinkPath;
                   relative -> filename:absname(filename:join(filename:dirname(Destination), SymlinkPath))
               end,
            case normalize_path(AbsoluteSymlinkPath) of
                Destination -> {error, {Path, {file, eloop}}};
                RealPath -> resolve_real_path({Source, RealPath}, Package, Fuel - 1)
            end;

        {error, enoent} ->
            {error, {Path, {file, enoent}}};

        {error, _} ->
            {ok, Destination}
    end.

-spec normalize_path(file:filename()) -> file:filename().
normalize_path(Path) ->
    normalize_path([], filename:split(Path)).

-spec normalize_path([file:filename()], [file:filename()]) -> file:filename().
normalize_path(NormalizedPath, []) ->
    filename:join(lists:reverse(NormalizedPath));
normalize_path(NormalizedPath, [H | T]) ->
    normalize_path(case H of
                       "."  -> NormalizedPath;
                       ".." -> tl(NormalizedPath);
                       _    -> [H | NormalizedPath]
                   end, T).

-spec is_erlang(file:filename()) -> boolean().
is_erlang(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?ERLANG_EXTENSIONS).

-spec is_beam(file:filename()) -> boolean().
is_beam(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?BEAM_EXTENSIONS).

-spec format_error(error()) -> io_lib:chars().
format_error({{SymlinkSrc, SymlinkDst}, Error}) ->
    atomizer:words(["Failed to read symlink", SymlinkSrc, "->", [SymlinkDst, ":"], atomizer:format_error(Error)]);
format_error({Path, Error}) ->
    atomizer:words(["Failed to read", [Path, ":"], atomizer:format_error(Error)]).
