-module(atomizer).

-include("atomizer.hrl").

-export([
    atomize/1,
    format_error/1
]).

-spec atomize(source()) -> {ok, atoms(), warnings()} | {error, {?MODULE, term()}}.
atomize(Source) ->
    Pid = spawn_link(atomizer_compare, compare, [self()]),
    spawn_link(atomizer_collect, collect, [self(), Source]),
    loop(Pid, maps:new(), sets:new(), -1).

-spec loop(pid(), atoms(), warnings(), integer()) -> {ok, atoms(), warnings(), non_neg_integer()} | {error, term()}.
loop(Pid, Atoms, Warnings, NrParsed) ->
    receive
        {atom, Atom, File, Position} ->
            Pid ! {atom, Atom},
            loop(Pid, add_atom_location(Atom, File, Position, Atoms), Warnings, NrParsed);

        {done_atoms, ActualNrParsed} ->
            Pid ! done_atoms,
            loop(Pid, Atoms, Warnings, ActualNrParsed);

        {warning, Atom, Btom, _} ->
            loop(Pid, Atoms, sets:add_element({Atom, Btom}, Warnings), NrParsed);

        done_warnings ->
            {ok, Atoms, Warnings, NrParsed};

        {error, Error} ->
            {error, {?MODULE, Error}}
    end.

-spec add_atom_location(atom(), file:filename(), position(), atoms()) -> atoms().
add_atom_location(Atom, File, Position, Atoms) ->
    Locations = maps:get(Atom, Atoms, maps:new()),
    Positions = maps:get(File, Locations, sets:new()),
    UpdatedPositions = sets:add_element(Position, Positions),
    UpdatedLocations = maps:put(File, UpdatedPositions, Locations),
    maps:put(Atom, UpdatedLocations, Atoms).

-spec format_error({module(), term()}) -> string().
format_error({Module, Error}) ->
    Module:format_error(Error).
