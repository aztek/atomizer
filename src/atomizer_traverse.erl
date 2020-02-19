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

-spec traverse(pid(), atomizer:source(), [file:filename()]) -> {done_source, atomizer:source()} | {error, {module(), term()}}.
traverse(Pid, Source, IncludePaths) ->
    case read_source(Pid, Source, IncludePaths) of
        ok -> Pid ! {done_source, Source};
        {error, {Module, Error}} ->
            case atomizer:cli_get_warn_errors() of
                true ->
                    atomizer:warning(Module:format_error(Error)),
                    Pid ! {done_source, Source};
                false ->
                    Pid ! {error, {Module, Error}}
            end
    end.

-spec read_source(pid(), atomizer:source(), [file:filename()]) -> ok | {error, {module(), term()}}.
read_source(Pid, Source, IncludePaths) ->
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
        {ok, other}  -> ignore;
        {ok, Source} -> Pid ! {add_source, Source};
        {error, {Module, Error}} ->
            case atomizer:cli_get_warn_errors() of
                true ->
                    atomizer:warning(Module:format_error(Error)),
                    ignore;
                false ->
                    {error, {Module, Error}}
            end
    end.

-spec detect_source(file:filename()) -> {ok, atomizer:source() | other} | {error, {?MODULE, error()}}.
detect_source(Path) ->
    case resolve_real_path(Path) of
        {ok, RealPath} ->
            case file:read_file_info(RealPath) of
                {ok, Info} ->
                    Type     = Info#file_info.type,
                    IsErlang = is_erlang(RealPath),
                    IsBeam   = is_beam(RealPath),
                    Source = if
                                 Type == directory         -> {dir,  Path};
                                 Type == regular, IsErlang -> {erl,  Path};
                                 Type == regular, IsBeam   -> {beam, Path};
                                 true -> other
                             end,
                    {ok, Source};

                {error, Error} ->
                    {error, {?MODULE, {{Path, RealPath}, {file, Error}}}}
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
            RealPath = filename:absname(SymlinkPath, filename:dirname(Destination)),
            resolve_real_path({Source, RealPath}, Fuel - 1);

        {error, enoent} ->
            {error, {Path, {file, enoent}}};

        {error, _} ->
            {ok, Destination}
    end.

-spec is_erlang(file:filename()) -> boolean().
is_erlang(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?ERLANG_EXTENSIONS).

-spec is_beam(file:filename()) -> boolean().
is_beam(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?BEAM_EXTENSIONS).

-spec format_error(error()) -> string().
format_error({{SymlinkSrc, SymlinkDst}, {Module, Error}}) ->
    io_lib:format("Failed to read symlink ~s -> ~s: ~s", [SymlinkSrc, SymlinkDst, Module:format_error(Error)]);
format_error({Path, {Module, Error}}) ->
    io_lib:format("Failed to read ~s: ~s", [Path, Module:format_error(Error)]).
