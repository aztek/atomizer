-module(atomizer).

-include("atomizer.hrl").

-export([
    atomize/3,
    format_error/1
]).

-type atomize_result() :: {ok, atoms(), warnings(), NrFiles :: non_neg_integer(), NrDirs :: non_neg_integer()}
                        | {error, {?MODULE, term()}}.

-spec atomize([file:filename()], [file:filename()], boolean()) -> atomize_result().
atomize(Paths, IncludePaths, ParseBeams) ->
    Pid = spawn_link(atomizer_compare, compare, [self()]),
    spawn_link(atomizer_collect, collect, [self(), Paths, IncludePaths, ParseBeams]),
    loop(Pid, maps:new(), sets:new(), {-1, -1}).

-spec loop(pid(), atoms(), warnings(), {integer(), integer()}) -> atomize_result().
loop(Pid, Atoms, Warnings, NrParsed) ->
    receive
        {atom, Atom, File, Position} ->
            Pid ! {atom, Atom},
            loop(Pid, add_atom_location(Atom, File, Position, Atoms), Warnings, NrParsed);

        {done_atoms, NrFiles, NrDirs} ->
            Pid ! done_atoms,
            loop(Pid, Atoms, Warnings, {NrFiles, NrDirs});

        {warning, Atom, Btom, _} ->
            loop(Pid, Atoms, sets:add_element({Atom, Btom}, Warnings), NrParsed);

        done_warnings ->
            {NrFiles, NrDirs} = NrParsed,
            {ok, Atoms, Warnings, NrFiles, NrDirs};

        {error, Error} ->
            case ?WARN_ERRORS of
                true ->
                    ?WARNING(format_error(Error)),
                    loop(Pid, Atoms, Warnings, NrParsed);
                false ->
                    {error, Error}
            end
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
