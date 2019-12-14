-module(atomizer).

-include("atomizer.hrl").

-export([atomize/1]).

-spec atomize(source()) -> {ok, atoms(), warnings()} | {error, term()}.
atomize(Source) ->
    put(comparison, spawn_link(atomizer_compare, compare, [self()])),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    loop(maps:new(), sets:new()).

-spec loop(atoms(), warnings()) -> {ok, atoms(), warnings()} | {error, atom()}.
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
            UpdatedWarnings = sets:add_element({Atom, Btom}, Warnings),
            loop(Atoms, UpdatedWarnings);

        done_warnings ->
            {ok, Atoms, Warnings};

        {error, Error} ->
            {error, Error}
    end.
