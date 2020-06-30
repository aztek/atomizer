-module(atomizer_traverse).

-include_lib("kernel/include/file.hrl").

-define(MAX_LEVEL_SYMLINKS, 10).
-define(ERLANG_EXTENSIONS, [".erl", ".hrl"]).
-define(BEAM_EXTENSIONS, [".beam"]).

-export([
    traverse/3,
    detect_source/2,
    format_error/1
]).

-type symlink() :: {Source :: file:filename(), Destination :: file:filename()}.
-type error()   :: {file:filename() | symlink(), {module(), atom()}}.

-spec traverse(pid(), atomizer:source(), atomizer:package()) -> {done_source, atomizer:source()} | {error, {module(), term()}}.
traverse(Pid, Source, Package) ->
    case read_source(Pid, Source, Package) of
        ok -> Pid ! {done_source, Source};
        {error, {Module, Error}} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(Module:format_error(Error)),
                    Pid ! {done_source, Source};
                false ->
                    Pid ! {error, {Module, Error}}
            end
    end.

-spec read_source(pid(), atomizer:source(), atomizer:package()) -> ok | {error, {module(), term()}}.
read_source(Pid, Source, Package) ->
    IncludePaths = atomizer:package_includes(Package),
    case Source of
        {erl,  File} -> atomizer_parse:parse_erl(Pid, File, IncludePaths);
        {beam, File} -> atomizer_parse:parse_beam(Pid, File);
        {dir,  Dir}  ->
            case file:list_dir(Dir) of
                {ok, Names}    -> traverse_paths(Pid, Package, [filename:join(Dir, Name) || Name <- Names]);
                {error, Error} -> {error, {?MODULE, {Dir, {file, Error}}}}
            end
    end.

-spec traverse_paths(pid(), atomizer:package(), [file:filename()]) -> any().
traverse_paths(Pid, Package, Paths) ->
    lists:foreach(fun (Path) -> traverse_path(Pid, Package, Path) end, Paths).

-spec traverse_path(pid(), atomizer:package(), file:filename()) -> ignore | {add_source, atomizer:source()} | {error, error()}.
traverse_path(Pid, Package, Path) ->
    case detect_source(Path, Package) of
        {ok, Source} -> Pid ! {add_source, Source};
        ignore -> ignore;
        {error, {Module, Error}} ->
            case atomizer_cli_options:get_warn_errors() of
                true ->
                    atomizer:warning(Module:format_error(Error)),
                    ignore;
                false ->
                    {error, {Module, Error}}
            end
    end.

-spec detect_source(file:filename(), atomizer:package()) -> {ok, atomizer:source()} | ignore | {error, {?MODULE, error()}}.
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
                            Type     = Info#file_info.type,
                            IsErlang = is_erlang(RealPath),
                            IsBeam   = is_beam(RealPath),
                            case Type of
                                directory             -> {ok, {dir,  RealPath}};
                                regular when IsErlang -> {ok, {erl,  RealPath}};
                                regular when IsBeam   -> {ok, {beam, RealPath}};
                                _                     -> ignore
                            end;

                        {error, Error} ->
                            {error, {?MODULE, {{Path, RealPath}, {file, Error}}}}
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
resolve_real_path(Path, Package, 0) -> {error, {Path, {file, eloop}}};
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
format_error({{SymlinkSrc, SymlinkDst}, {Module, Error}}) ->
    atomizer:words(["Failed to read symlink", SymlinkSrc, "->", [SymlinkDst, ":"], Module:format_error(Error)]);
format_error({Path, {Module, Error}}) ->
    atomizer:words(["Failed to read", [Path, ":"], Module:format_error(Error)]).
