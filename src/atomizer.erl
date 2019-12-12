-module(atomizer).

-export_type([source/0, position/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type position() :: pos_integer().

-export([atomize/3]).

-spec atomize(source(), ets:tid(), ets:tid()) -> ok | {error, term()}.
atomize(Source, AtomsTable, WarningsTable) ->
    put(atoms_table, AtomsTable),
    put(warnings_table, WarningsTable),
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    loop().

-spec loop() -> ok | {error, atom()}.
loop() ->
    receive
        {atom, Atom, File, Position} ->
            get(comparison) ! {atom, Atom},
            ets:insert(get(atoms_table), {Atom, {File, Position}}),
            loop();

        done_atoms ->
            get(comparison) ! done_atoms,
            loop();

        {warning, Atom, Btom, _} ->
            ets:insert(get(warnings_table), {{Atom, Btom}}),
            loop();

        done_warnings ->
            ok;

        {error, Error} ->
            {error, Error}
    end.
