#!/usr/bin/env escript

-module(atomizer).

-mode(compile).

-export([main/1]).

-type atoms() :: [{atom(), Line :: pos_integer()}].
-type location() :: {file:filename(), Line :: pos_integer()}.
-type bag(Key, Value) :: #{Key => [Value]}.
-type result(Value) :: {ok, Value} | {error, term()}.

-spec main([string()]) -> no_return().
main(CmdArgs) ->
  case atomize_files(CmdArgs) of
    {ok, Atoms} ->
      display_atoms(Atoms),
      erlang:halt(0, [{flush, true}]);
    {error, Error} ->
      io:format(standard_error, "Error: ~s~n", [Error]),
      erlang:halt(1, [{flush, true}])
  end.

-spec append_bags(bag(Key, Value), bag(Key, Value)) -> bag(Key, Value) when
  Key   :: term(),
  Value :: term().
append_bags(Bag1, Bag2) ->
  Keys = sets:to_list(sets:from_list(maps:keys(Bag1) ++ maps:keys(Bag2))),
  Append = fun (Key) -> {Key, maps:get(Key, Bag1, []) ++ maps:get(Key, Bag2, [])} end,
  maps:from_list(lists:map(Append, Keys)).

-spec display_atoms(bag(atom(), location())) -> ok.
display_atoms(Atoms) ->
  lists:foreach(fun (Atom) -> display_atom(Atom, maps:get(Atom, Atoms)) end,
                lists:sort(maps:keys(Atoms))).

-spec display_atom(atom(), [location()]) -> ok.
display_atom(Atom, Locations) ->
  io:format("~p~n", [Atom]),
  lists:foreach(fun ({F, L}) -> io:format("~s:~p~n", [filename:absname(F), L]) end,
                lists:sort(Locations)),
  io:format("~n").

-spec atomize_files([file:filename()]) -> result(bag(atom(), location())).
atomize_files(FileNames) ->
  lists:foldl(fun (FileName, Bag) ->
                case Bag of
                  {ok, Bag0} -> case atomize_file(FileName) of
                                  {ok, Bag1} -> {ok, append_bags(Bag0, Bag1)};
                                  Error -> Error
                                end;
                  Error -> Error
                end
              end, {ok, #{}}, FileNames).

-spec atomize_file(FileName :: file:filename()) -> result(bag(atom(), location())).
atomize_file(FileName) ->
  Atomize = fun (Forms) ->
    Atoms = lists:flatmap(fun atomize_form/1, Forms),
    to_bag(lists:map(fun ({Atom, Line}) -> {Atom, {FileName, Line}} end, Atoms))
  end,
  case epp:parse_file(FileName, []) of
    {ok, Forms}    -> {ok, Atomize(Forms)};
    {ok, Forms, _} -> {ok, Atomize(Forms)};
    {error, Error} -> {error, Error}
  end.

-spec to_bag([{Key, Value :: term()}]) -> bag(Key, Value) when
  Key   :: term(),
  Value :: term().
to_bag(List) ->
  lists:foldl(fun ({Key, Value}, Bag) ->
                Values = maps:get(Key, Bag, []),
                maps:put(Key, [Value|Values], Bag)
              end, #{}, List).

-spec atomize_form(erl_parse:abstract_form() | erl_parse:form_info()) -> atoms().
atomize_form({attribute, _, module,      _}) -> [];
atomize_form({attribute, _, behavior,    _}) -> [];
atomize_form({attribute, _, behaviour,   _}) -> [];
atomize_form({attribute, _, export,      _}) -> [];
atomize_form({attribute, _, import,      _}) -> [];
atomize_form({attribute, _, export_type, _}) -> [];
atomize_form({attribute, _, compile,     _}) -> [];
atomize_form({attribute, _, file,        _}) -> [];
atomize_form({attribute, _, record,   {_, Fs  }}) -> lists:flatmap(fun atomize_record_field_decl/1, Fs);
atomize_form({attribute, _, type,     {_, T, _}}) -> atomize_type(T);
atomize_form({attribute, _, opaque,   {_, T, _}}) -> atomize_type(T);
atomize_form({attribute, _, callback, {_, Ts  }}) -> lists:flatmap(fun atomize_function_type/1, Ts);
atomize_form({attribute, _, spec,     {_, Ts  }}) -> lists:flatmap(fun atomize_function_type/1, Ts);
atomize_form({attribute, _, _,                _}) -> []; %% TODO: Should we parse custom module attributes?
atomize_form({function,  _, _, _, Cs}) -> lists:flatmap(fun atomize_clause/1, Cs);
atomize_form({eof, _}) -> [];
atomize_form({error,   Info}) -> io:format("Error: ~w~n",   [Info]), [];
atomize_form({warning, Info}) -> io:format("Warning: ~w~n", [Info]), [].

-spec atomize_expr(erl_parse:abstract_expr()) -> atoms().
atomize_expr({atom,         L,        A}) -> [{A, L}];
atomize_expr({char,         _,        _}) -> [];
atomize_expr({float,        _,        _}) -> [];
atomize_expr({integer,      _,        _}) -> [];
atomize_expr({string,       _,        _}) -> [];
atomize_expr({match,        _,     P, E}) -> atomize_pattern(P) ++ atomize_expr(E);
atomize_expr({var,          _,        _}) -> [];
atomize_expr({tuple,        _,       Es}) -> lists:flatmap(fun atomize_expr/1, Es);
atomize_expr({nil,          _          }) -> [];
atomize_expr({cons,         _,     H, T}) -> atomize_expr(H) ++ atomize_expr(T);
atomize_expr({bin,          _,      BEs}) -> lists:flatmap(fun atomize_binelement/1, BEs);
atomize_expr({op,           _, _,  A, B}) -> atomize_expr(A) ++ atomize_expr(B);
atomize_expr({op,           _, _,     E}) -> atomize_expr(E);
atomize_expr({record,       _,    _, Fs}) -> lists:flatmap(fun atomize_record_field/1, Fs);
atomize_expr({record,       _, E, _, Fs}) -> atomize_expr(E) ++ lists:flatmap(fun atomize_record_field/1, Fs);
atomize_expr({remote,       _, {atom, _, _}, {atom, _, _}}) -> [];
atomize_expr({remote,       _, A, {atom, _, _}}) -> atomize_expr(A);
atomize_expr({remote,       _, {atom, _, _}, B}) -> atomize_expr(B);
atomize_expr({remote,       _,     A, B}) -> atomize_expr(A) ++ atomize_expr(B);
atomize_expr({record_index, _,    _,  _}) -> [];
atomize_expr({record_field, _, E, _,  _}) -> atomize_expr(E);
atomize_expr({map,          _,       As}) -> lists:flatmap(fun atomize_assoc/1, As);
atomize_expr({map,          _, E,    As}) -> atomize_expr(E) ++ lists:flatmap(fun atomize_assoc/1, As);
atomize_expr({'catch',      _, E       }) -> atomize_expr(E);
atomize_expr({call,         _, {atom, _, _}, Es}) -> lists:flatmap(fun atomize_expr/1, Es);
atomize_expr({call,         _, E,    Es}) -> atomize_expr(E) ++ lists:flatmap(fun atomize_expr/1, Es);
atomize_expr({lc,           _,    T, Qs}) -> atomize_expr(T) ++ lists:flatmap(fun atomize_qualifier/1, Qs);
atomize_expr({bc,           _,    T, Qs}) -> atomize_expr(T) ++ lists:flatmap(fun atomize_qualifier/1, Qs);
atomize_expr({block,        _,       Es}) -> lists:flatmap(fun atomize_expr/1, Es);
atomize_expr({'if',         _,       Cs}) -> lists:flatmap(fun atomize_clause/1, Cs);
atomize_expr({'case',       _,    E, Cs}) -> atomize_expr(E) ++ lists:flatmap(fun atomize_clause/1, Cs);
atomize_expr({'try',        _, Es, Scs, Ccs, As}) -> lists:flatmap(fun atomize_expr/1, Es ++ As) ++ lists:flatmap(fun atomize_clause/1, Scs ++ Ccs);
atomize_expr({'receive',    _, Cs       }) -> lists:flatmap(fun atomize_clause/1, Cs);
atomize_expr({'receive',    _, Cs, E, Es}) -> lists:flatmap(fun atomize_clause/1, Cs) ++ atomize_expr(E) ++ lists:flatmap(fun atomize_expr/1, Es);
atomize_expr({'fun',        _, {function, _, _}})    -> [];
atomize_expr({'fun',        _, {function, _, _, _}}) -> [];
atomize_expr({'fun',        _, {clauses, Cs}}) -> lists:flatmap(fun atomize_clause/1, Cs);
atomize_expr({named_fun,    _, _, Cs}) -> lists:flatmap(fun atomize_clause/1, Cs).

-spec atomize_binelement(erl_parse:af_binelement(erl_parse:abstract_expr())) -> atoms().
atomize_binelement({bin_element, _, E, default, _}) -> atomize_expr(E);
atomize_binelement({bin_element, _, E, S,       _}) -> atomize_expr(E) ++ atomize_expr(S).

-spec atomize_binelement_pattern(erl_parse:af_binelement(erl_parse:af_pattern())) -> atoms().
atomize_binelement_pattern({bin_element, _, E, default, _}) -> atomize_pattern(E);
atomize_binelement_pattern({bin_element, _, E, S,       _}) -> atomize_pattern(E) ++ atomize_pattern(S).

-spec atomize_record_field_decl(erl_parse:af_field_decl()) -> atoms().
atomize_record_field_decl({typed_record_field, F, T}) -> atomize_record_field(F) ++ atomize_type(T);
atomize_record_field_decl(F) -> atomize_record_field(F).

-spec atomize_record_field(erl_parse:af_field()) -> atoms().
atomize_record_field({record_field, _, _})    -> [];
atomize_record_field({record_field, _, _, E}) -> atomize_expr(E).

-spec atomize_clause(erl_parse:af_clause()) -> atoms().
atomize_clause({clause, _, Ps, Gs, Es}) ->
  lists:flatmap(fun atomize_pattern/1, Ps) ++
  lists:flatmap(fun atomize_guard/1, lists:flatten(Gs)) ++
  lists:flatmap(fun atomize_expr/1, Es).

-spec atomize_pattern(erl_parse:af_pattern()) -> atoms().
atomize_pattern({atom,         L,         A}) -> [{A, L}];
atomize_pattern({char,         _,         _}) -> [];
atomize_pattern({float,        _,         _}) -> [];
atomize_pattern({integer,      _,         _}) -> [];
atomize_pattern({string,       _,         _}) -> [];
atomize_pattern({match,        _,    P1, P2}) -> atomize_pattern(P1) ++ atomize_pattern(P2);
atomize_pattern({var,          _,         _}) -> [];
atomize_pattern({tuple,        _,        Ps}) -> lists:flatmap(fun atomize_pattern/1, Ps);
atomize_pattern({nil,          _           }) -> [];
atomize_pattern({cons,         _,    P1, P2}) -> atomize_pattern(P1) ++ atomize_pattern(P2);
atomize_pattern({bin,          _,        Bs}) -> lists:flatmap(fun atomize_binelement_pattern/1, Bs);
atomize_pattern({op,           _, _, P1, P2}) -> atomize_pattern(P1) ++ atomize_pattern(P2);
atomize_pattern({op,           _, _,      P}) -> atomize_pattern(P);
atomize_pattern({record,       _,     _, Fs}) -> lists:flatmap(fun atomize_record_field/1, Fs);
atomize_pattern({record_index, _,      _, _}) -> [];
atomize_pattern({map,          _,        As}) -> lists:flatmap(fun atomize_assoc/1, As).

-spec atomize_guard(erl_parse:af_guard_test()) -> atoms().
atomize_guard({atom,         L,         A}) -> [{A, L}];
atomize_guard({char,         _,         _}) -> [];
atomize_guard({float,        _,         _}) -> [];
atomize_guard({integer,      _,         _}) -> [];
atomize_guard({string,       _,         _}) -> [];
atomize_guard({var,          _,         _}) -> [];
atomize_guard({tuple,        _,        Gs}) -> lists:flatmap(fun atomize_guard/1, Gs);
atomize_guard({nil,          _           }) -> [];
atomize_guard({cons,         _,    G1, G2}) -> atomize_guard(G1) ++ atomize_guard(G2);
atomize_guard({bin,          _,       BEs}) -> lists:flatmap(fun atomize_binelement/1, BEs);
atomize_guard({op,           _, _, G1, G2}) -> atomize_guard(G1) ++ atomize_guard(G2);
atomize_guard({op,           _, _,      G}) -> atomize_guard(G);
atomize_guard({record,       _,     _, Fs}) -> lists:flatmap(fun atomize_record_field_guard/1, Fs);
atomize_guard({record_index, _,      _, _}) -> [];
atomize_guard({record_field, _,   G, _, _}) -> atomize_guard(G);
atomize_guard({map,          _,        As}) -> lists:flatmap(fun atomize_assoc/1, As);
atomize_guard({map,          _,     G, As}) -> atomize_guard(G) ++ lists:flatmap(fun atomize_assoc/1, As);
atomize_guard({call,         _,     _, Gs}) -> lists:flatmap(fun atomize_guard/1, Gs).

-spec atomize_record_field_guard(erl_parse:af_record_field(erl_parse:af_guard_test())) -> atoms().
atomize_record_field_guard({record_field, _, _, G}) -> atomize_guard(G).

-spec atomize_assoc(erl_parse:af_assoc(erl_parse:abstract_expr())) -> atoms().
atomize_assoc({map_field_assoc, _, K, V}) -> atomize_expr(K) ++ atomize_expr(V);
atomize_assoc({map_field_exact, _, K, V}) -> atomize_expr(K) ++ atomize_expr(V).

-spec atomize_qualifier(erl_parse:af_qualifier()) -> atoms().
atomize_qualifier({generate,   _, P, E}) -> atomize_pattern(P) ++ atomize_expr(E);
atomize_qualifier({b_generate, _, P, E}) -> atomize_pattern(P) ++ atomize_expr(E);
atomize_qualifier(E) -> atomize_expr(E).

-spec atomize_type(erl_parse:abstract_type()) -> atoms().
atomize_type({type, _, any})                 -> [];
atomize_type({type, _, binary,           _}) -> [];
atomize_type({type, _, nil,              _}) -> [];
atomize_type({type, _, 'fun',           Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, product,         Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, range,            _}) -> [];
atomize_type({type, _, map,            any}) -> [];
atomize_type({type, _, map_field_assoc, Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, map_field_exact, Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, record,           _}) -> [];
atomize_type({type, _, field_type,       _}) -> [];
atomize_type({type, _, tuple,          any}) -> [];
atomize_type({type, _, tuple,           Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, union,           Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({type, _, _,               Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({ann_type,    _,           Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
atomize_type({remote_type, _,            _}) -> [];
atomize_type({atom,        L,            A}) -> [{A, L}];
atomize_type({char,        _,            _}) -> [];
atomize_type({integer,     _,            _}) -> [];
atomize_type({op,          _,         _, _}) -> [];
atomize_type({op,          _,      _, _, _}) -> [];
atomize_type({var,         _,            _}) -> [];
atomize_type({user_type,   _,        _, Ts}) -> lists:flatmap(fun atomize_type/1, Ts).

-spec atomize_function_type(FunctionType) -> atoms() when
  FunctionType :: erl_parse:af_constrained_function_type() | erl_parse:af_function_type().
atomize_function_type({type, _, bounded_fun, Ts}) ->
  lists:flatmap(fun (Tts) when is_list(Tts) -> lists:flatmap(fun atomize_constraint/1, Tts);
                    (T) -> atomize_function_type(T)
                end, Ts);
atomize_function_type({type, _, 'fun', Ps}) ->
  lists:flatmap(fun ({type, _, product, Ts}) -> lists:flatmap(fun atomize_type/1, Ts);
                    (T) -> atomize_type(T)
                end, Ps).

-spec atomize_constraint(erl_parse:af_constraint()) -> atoms().
atomize_constraint({type, _, constraint, [{atom, _, is_subtype}, Ts]}) ->
  lists:flatmap(fun ({var, _, _}) -> [];
                    (T) -> atomize_type(T)
                end, Ts).
