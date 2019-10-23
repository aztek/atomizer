-module(atomizer).

-export_type([location/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type location() :: {file:filename(), Line :: pos_integer()}.

-export([atomize/1]).

-spec atomize(source()) -> {ok, Atoms, Warnings} | {error, term()} when
    Atoms    :: multimaps:multimap(atom(), location()),
    Warnings :: sets:set({atom(), atom()}).
atomize(Source) ->
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    spawn_link(atomizer_collect, init, [self(), Source]),
    loop(multimaps:empty(), sets:new()).

-spec loop(Atoms, Warnings) -> {ok, Atoms, Warnings} | {error, atom()} when
    Atoms    :: multimaps:multimap(atom(), location()),
    Warnings :: sets:set({atom(), atom()}).
loop(Atoms, Warnings) ->
    receive
        {atom, Atom, File, Location} ->
            get(comparison) ! {atom, Atom},
            loop(multimaps:put(Atom, {File, Location}, Atoms), Warnings);

        done_atoms ->
            get(comparison) ! done_atoms,
            loop(Atoms, Warnings);

        {warning, Atom, Btom, _} ->
            loop(Atoms, sets:add_element({Atom, Btom}, Warnings));

        done_warnings ->
            {ok, Atoms, Warnings};

        {error, Error} ->
            {error, Error}
    end.
