-module(atomizer_collect).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export([collect/2]).

-spec collect(pid(), source()) -> ok | {error, term()}.
collect(Callback, Source) ->
    put(callback, Callback),
    Queue = case Source of
                {file,  File}  -> [{erl, File}];
                {dir,   Dir}   -> [{dir, Dir}];
                {files, Files} -> lists:map(fun (File) -> {erl, File} end, Files);
                {dirs,  Dirs}  -> lists:map(fun (Dir)  -> {dir, Dir}  end, Dirs)
            end,
    loop(sets:new(), queue:from_list(Queue)).

-spec loop(Pool, Queue) -> ok | {error, term()} when
    Pool  :: sets:set(atomizer_parse:path()),
    Queue :: queue:queue(atomizer_parse:path()).
loop(Pool, Queue) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            get(callback) ! done_atoms,
            ok;

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Path}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_parse, parse_path, [self(), Path]),
            loop(sets:add_element(Path, Pool), TailQueue);

        _ ->
            receive
                {add_path, Path} ->
                    loop(Pool, queue:in(Path, Queue));

                {atom, Atom, File, Location} ->
                    get(callback) ! {atom, Atom, File, Location},
                    loop(Pool, Queue);

                {done_path, Path} ->
                    loop(sets:del_element(Path, Pool), Queue);

                {error, Error} ->
                    get(callback) ! {error, Error}
            end
    end.
