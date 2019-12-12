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
    Warnings :: [{atom(), atom()}].
atomize(Source) ->
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    ets:new(warnings, [private, named_table, set]),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    Result = loop(maps:new()),
    ets:delete(warnings),
    Result.

-spec loop(Atoms) -> {ok, Atoms, Warnings} | {error, atom()} when
    Atoms    :: atoms(),
    Warnings :: [{atom(), atom()}].
loop(Atoms) ->
    receive
        {atom, Atom, File, Position} ->
            get(comparison) ! {atom, Atom},
            Locations = maps:get(Atom, Atoms, maps:new()),
            Positions = maps:get(File, Locations, sets:new()),
            UpdatedPositions = sets:add_element(Position, Positions),
            UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
            UpdatedAtoms = maps:put(Atom, UpdatedLocations, Atoms),
            loop(UpdatedAtoms);

        done_atoms ->
            get(comparison) ! done_atoms,
            loop(Atoms);

        {warning, Atom, Btom, _} ->
            ets:insert(warnings, {{Atom, Btom}}),
            loop(Atoms);

        done_warnings ->
            Warnings = lists:map(fun ({W}) -> W end, ets:tab2list(warnings)),
            {ok, Atoms, Warnings};

        {error, Error} ->
            {error, Error}
    end.
