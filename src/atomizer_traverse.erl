-module(atomizer_traverse).

-include_lib("kernel/include/file.hrl").

-define(MAX_LEVEL_SYMLINKS, 10).
-define(ERLANG_EXTENSIONS, [".erl", ".hrl"]).
-define(BEAM_EXTENSIONS, [".beam"]).

-export([
    traverse/3,
    detect_source/1,
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
                {ok, Names}    -> traverse_paths(Pid, [filename:join(Dir, Name) || Name <- Names]);
                {error, Error} -> {error, {?MODULE, {Dir, {file, Error}}}}
            end
    end.

-spec traverse_paths(pid(), [file:filename()]) -> any().
traverse_paths(Pid, Paths) ->
    lists:foreach(fun (Path) -> traverse_path(Pid, Path) end, Paths).

-spec traverse_path(pid(), file:filename()) -> ignore | {add_source, atomizer:source()} | {error, error()}.
traverse_path(Pid, Path) ->
    case detect_source(Path) of
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

-spec detect_source(file:filename()) -> {ok, atomizer:source()} | ignore | {error, {?MODULE, error()}}.
detect_source(Path) ->
    case resolve_real_path(Path) of
        {ok, RealPath} ->
            Basename  = filename:basename(RealPath),
            Ignores   = atomizer:global_ignores(),
            IsIgnored = lists:member(Basename, Ignores),
            case IsIgnored of
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

        {error, Error} ->
            {error, {?MODULE, Error}}
    end.

-spec resolve_real_path(file:filename()) -> {ok, file:filename()} | {error, error()}.
resolve_real_path(Path) -> resolve_real_path(Path, ?MAX_LEVEL_SYMLINKS).

-spec resolve_real_path(file:filename() | symlink(), Fuel :: non_neg_integer()) -> {ok, file:filename()} | {error, error()}.
resolve_real_path(Path, 0) -> {error, {Path, {file, eloop}}};
resolve_real_path(Path, Fuel) ->
    {Source, Destination} = case Path of {_, _} -> Path; _ -> {Path, Path} end,
    case file:read_link_all(Destination) of
        {ok, SymlinkPath} ->
            AbsoluteSymlinkPath =
                case filename:pathtype(SymlinkPath) of
                   absolute -> SymlinkPath;
                   relative -> filename:absname(filename:join(filename:dirname(Destination), SymlinkPath))
               end,
            case normalize_path(AbsoluteSymlinkPath) of
                Destination -> {error, {Path, {file, eloop}}};
                RealPath -> resolve_real_path({Source, RealPath}, Fuel - 1)
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
