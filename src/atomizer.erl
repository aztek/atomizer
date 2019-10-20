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

-spec atomize(source()) -> {ok, Atoms, Warnings} | {error, term()} when
    Atoms :: multimaps:multimap(atom(), location()),
    Warnings :: [{atom(), atom()}].
atomize(Source) ->
    Queue = case Source of
                {file,  File}  -> [{erl, File}];
                {dir,   Dir}   -> [{dir, Dir}];
                {files, Files} -> lists:map(fun (File) -> {erl, File} end, Files);
                {dirs,  Dirs}  -> lists:map(fun (Dir)  -> {dir, Dir}  end, Dirs)
            end,
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    loop(sets:new(), queue:from_list(Queue), multimaps:empty(), sets:new(), true).

-spec loop(Pool, Queue, Atoms, Warnings, ExpectWarnings) -> {ok, Atoms, Warnings} | {error, term()} when
    Pool  :: sets:set(atomizer_parse:path()),
    Queue :: queue:queue(atomizer_parse:path()),
    Atoms :: multimaps:multimap(atom(), location()),
    Warnings :: set:set({atom(), atom()}),
    ExpectWarnings :: boolean().
loop(Pool, Queue, Atoms, Warnings, ExpectWarnings) ->
    case {sets:size(Pool), queue:len(Queue), ExpectWarnings} of
        {0, 0, false} ->
            {ok, Atoms, Warnings};

        {NrTakenDescriptors, QueueSize, _} when NrTakenDescriptors < ?OPEN_FILE_LIMIT, QueueSize > 0 ->
            {{value, Path}, TailQueue} = queue:out(Queue),
            spawn_link(atomizer_parse, parse_path, [self(), Path]),
            loop(sets:add_element(Path, Pool), TailQueue, Atoms, Warnings, ExpectWarnings);

        {PoolSize, QueueSize, _} ->
            case {PoolSize, QueueSize} of
                {0, 0} -> get(comparison) ! done_atoms;
                _ -> ignore
            end,
            receive
                {add_path, Path} ->
                    loop(Pool, queue:in(Path, Queue), Atoms, Warnings, ExpectWarnings);

                {atom, Atom, File, Location} ->
                    get(comparison) ! {atom, Atom},
                    loop(Pool, Queue, multimaps:put(Atom, {File, Location}, Atoms), Warnings, ExpectWarnings);

                {done_path, Path} ->
                    loop(sets:del_element(Path, Pool), Queue, Atoms, Warnings, ExpectWarnings);

                {warn, Atom, Btom, _} ->
                    loop(Pool, Queue, Atoms, sets:add_element({Atom, Btom}, Warnings), ExpectWarnings);

                done_warnings ->
                    loop(Pool, Queue, Atoms, Warnings, false);

                {error, Error} ->
                    {error, Error}
            end
    end.
