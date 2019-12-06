-module(atomizer).

-export_type([position/0, location/0, locations/0, atoms/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type position()  :: pos_integer().
-type location()  :: {file:filename(), position()}.
-type locations() :: multimaps:multimap(file:filename(), position()).
-type atoms()     :: #{atom() => locations()}.

-export([atomize/1]).

-spec atomize(source()) -> {ok, Atoms, Warnings} | {error, term()} when
    Atoms    :: atoms(),
    Warnings :: sets:set({atom(), atom()}).
atomize(Source) ->
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    loop(maps:new(), sets:new()).

-spec loop(Atoms, Warnings) -> {ok, Atoms, Warnings} | {error, atom()} when
    Atoms    :: atoms(),
    Warnings :: sets:set({atom(), atom()}).
loop(Atoms, Warnings) ->
    receive
        {atom, Atom, File, Position} ->
            get(comparison) ! {atom, Atom},
            Locations = maps:get(Atom, Atoms, maps:new()),
            Positions = maps:get(File, Locations, sets:new()),
            UpdatedPositions = sets:add_element(Position, Positions),
            UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
            UpdatedAtoms = maps:put(Atom, UpdatedLocations, Atoms),
            loop(UpdatedAtoms, Warnings);

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
