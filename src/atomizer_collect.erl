-module(atomizer_collect).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([collect/2]).

-spec collect(pid(), source()) -> ok | {error, term()}.
collect(Pid, Source) ->
    loop(Pid, sets:new(), queue:from_list(source_to_paths(Source))).

-spec source_to_paths(source()) -> [path()].
source_to_paths({file,  File})  -> [{erl, File}];
source_to_paths({dir,   Dir})   -> [{dir, Dir}];
source_to_paths({files, Files}) -> lists:map(fun (File) -> {erl, File} end, Files);
source_to_paths({dirs,  Dirs})  -> lists:map(fun (Dir)  -> {dir, Dir}  end, Dirs).

-spec loop(pid(), Pool :: sets:set(path()), Queue :: queue:queue(path())) -> ok | {error, term()}.
loop(Pid, Pool, Queue) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            Pid ! done_atoms,
            ok;

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Path}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_parse, parse_path, [self(), Path]),
            loop(Pid, sets:add_element(Path, Pool), TailQueue);

        _ ->
            receive
                {add_path, Path} ->
                    loop(Pid, Pool, queue:in(Path, Queue));

                {atom, Atom, File, Location} ->
                    Pid ! {atom, Atom, File, Location},
                    loop(Pid, Pool, Queue);

                {done_path, Path} ->
                    loop(Pid, sets:del_element(Path, Pool), Queue);

                {error, Error} ->
                    Pid ! {error, Error}
            end
    end.
