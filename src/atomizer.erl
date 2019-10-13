-module(atomizer).

-include_lib("kernel/include/file.hrl").

-define(ERLANG_EXTENSIONS, [".erl", ".hrl", ".yrl"]).

-export_type([location/0, result/1]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type location() :: {file:filename(), Line :: pos_integer()}.

-export([atomize/1]).

-spec atomize(source()) -> {ok, bags:bag(atom(), location())} | {error, term()}.
atomize(Source) ->
    Pid = self(),
    Callback = spawn_link(fun () -> init(Pid) end),
    ok = collect_files(Callback, Source),
    Callback ! done_collecting_files,
    receive
        Msg -> Msg
    end.

-record(state, {
    expect_files = true         :: boolean(),
    file_names   = sets:new()   :: sets:set(file:filename()),
    atoms        = bags:empty() :: bags:bag(atom(), location())
}).

-spec init(pid()) -> ok.
init(Callback) ->
    put(callback, Callback),
    loop(#state{}).

-spec loop(#state{}) -> ok.
loop(State) ->
    #state{
        expect_files = ExpectFiles,
        file_names   = FileNames,
        atoms        = Atoms
    } = State,
    case sets:is_empty(FileNames) and (ExpectFiles == false) of
        true ->
            get(callback) ! {ok, Atoms},
            ok;
        false ->
            receive
                {file, FileName} ->
                    spawn_link(atomizer_parse, parse_file, [self(), FileName]),
                    loop(State#state{file_names = sets:add_element(FileName, FileNames)});

                {atom, Atom, FileName, Location} ->
                    loop(State#state{atoms = bags:put(Atom, {FileName, Location}, Atoms)});

                {done_parsing_file, FileName} ->
                    loop(State#state{file_names = sets:del_element(FileName, FileNames)});

                done_collecting_files ->
                    loop(State#state{expect_files = false});

                {error, Error} ->
                    get(callback) ! {error, Error},
                    ok
            end
    end.

-spec collect_files(Callback :: pid(), source()) -> ok.
collect_files(Callback, {file, File}) ->
    Callback ! {file, File},
    ok;

collect_files(Callback, {dir, Dir}) ->
    {Files, Dirs} = list_dir(Dir),
    collect_files(Callback, {files, Files}),
    collect_files(Callback, {dirs, Dirs});

collect_files(Callback, {files, Files}) ->
    lists:foreach(fun (File) -> collect_files(Callback, {file, File}) end, Files),
    ok;

collect_files(Callback, {dirs, Dirs}) ->
    lists:foreach(fun (Dir) -> collect_files(Callback, {dir, Dir}) end, Dirs),
    ok.

-spec list_dir(Dir) -> {Files :: [file:filename()], Dirs :: [file:filename()]}.
list_dir(Dir) ->
    {ok, Names} = file:list_dir(Dir),
    lists:foldl(fun (Name, {Files, Dirs}) ->
                    Path = Dir ++ "/" ++ Name,
                    {ok, Info} = file:read_file_info(Path),
                    case Info#file_info.type of
                        directory -> {Files, [Path | Dirs]};
                        regular ->
                            case is_erlang(Name) of
                                true  -> {[Path | Files], Dirs};
                                false -> {Files, Dirs}
                            end
                    end
                end, {[], []}, Names).

-spec is_erlang(file:filename()) -> boolean().
is_erlang(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?ERLANG_EXTENSIONS).
