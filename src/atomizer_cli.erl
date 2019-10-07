-module(atomizer_cli).

-export([main/1]).

-spec main([string()]) -> no_return().
main(CmdArgs) ->
  case atomizer:atomize_files(CmdArgs) of
    {ok, Atoms} ->
      display_atoms(Atoms);
    {error, Error} ->
      io:format(standard_error, "Error: ~s~n", [Error])
  end.

-spec display_atoms(bags:bag(atom(), atomizer:location())) -> ok.
display_atoms(Atoms) ->
  lists:foreach(fun (Atom) -> display_atom(Atom, maps:get(Atom, Atoms)) end,
                lists:sort(maps:keys(Atoms))).

-spec display_atom(atom(), sets:set(atomizer:location())) -> ok.
display_atom(Atom, Locations) ->
  io:format("~p~n", [Atom]),
  lists:foreach(fun display_location/1, lists:sort(sets:to_list(Locations))),
  io:format("~n").

-spec display_location(atomizer:location()) -> ok.
display_location({F, L}) -> io:format("~s:~p~n", [filename:absname(F), L]).
