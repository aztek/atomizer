-module(atomizer).

-include_lib("kernel/include/file.hrl").

-define(OPEN_FILE_LIMIT, 10).

-export_type([location/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type location() :: {file:filename(), Line :: pos_integer()}.

-export([atomize/1]).

-spec atomize(source()) -> {ok, multimaps:multimap(atom(), location())} | {error, term()}.
atomize(Source) ->
    Queue = case Source of
                {file,  File}  -> [{erl, File}];
                {dir,   Dir}   -> [{dir, Dir}];
                {files, Files} -> lists:map(fun (File) -> {erl, File} end, Files);
                {dirs,  Dirs}  -> lists:map(fun (Dir)  -> {dir, Dir}  end, Dirs)
            end,
    loop(sets:new(), queue:from_list(Queue), multimaps:empty()).

-spec loop(Pool, Queue, Atoms) -> {ok, Atoms} | {error, term()} when
    Pool  :: sets:set(atomizer_parse:path()),
    Queue :: queue:queue(atomizer_parse:path()),
    Atoms :: multimaps:multimap(atom(), location()).
loop(Pool, Queue, Atoms) ->
    case {sets:size(Pool), queue:len(Queue)} of
        {0, 0} ->
            {ok, Atoms};

        {NrTakenDescriptors, QueueSize} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Path}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_parse, parse_path, [self(), Path]),
            loop(sets:add_element(Path, Pool), TailQueue, Atoms);

        _ ->
            receive
                {add_path, Path} ->
                    loop(Pool, queue:in(Path, Queue), Atoms);

                {atom, Atom, File, Location} ->
                    loop(Pool, Queue, multimaps:put(Atom, {File, Location}, Atoms));

                {done_path, Path} ->
                    loop(sets:del_element(Path, Pool), Queue, Atoms);

                {error, Error} ->
                    {error, Error}
            end
    end.
