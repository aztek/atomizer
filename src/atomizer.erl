-module(atomizer).

-include("atomizer.hrl").

-export([atomize/1]).

-spec atomize(source()) -> {ok, atoms(), warnings()} | {error, term()}.
atomize(Source) ->
    Pid = spawn_link(atomizer_compare, compare, [self()]),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    loop(Pid, maps:new(), sets:new()).

-spec loop(pid(), atoms(), warnings()) -> {ok, atoms(), warnings()} | {error, atom()}.
loop(Pid, Atoms, Warnings) ->
    receive
        {atom, Atom, File, Position} ->
            Pid ! {atom, Atom},
            loop(Pid, add_atom_location(Atom, File, Position, Atoms), Warnings);

        done_atoms ->
            Pid ! done_atoms,
            loop(Pid, Atoms, Warnings);

        {warning, Atom, Btom, _} ->
            loop(Pid, Atoms, sets:add_element({Atom, Btom}, Warnings));

        done_warnings ->
            {ok, Atoms, Warnings};

        {error, Error} ->
            {error, Error}
    end.

-spec add_atom_location(atom(), file:filename(), position(), atoms()) -> atoms().
add_atom_location(Atom, File, Position, Atoms) ->
    Locations = maps:get(Atom, Atoms, maps:new()),
    Positions = maps:get(File, Locations, sets:new()),
    UpdatedPositions = sets:add_element(Position, Positions),
    UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
    maps:put(Atom, UpdatedLocations, Atoms).
