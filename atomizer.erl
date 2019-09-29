#!/usr/bin/env escript

-module(atomizer).

-mode(compile).

-export([main/1]).

-type atoms()         :: [{atom(), Line :: pos_integer()}].
-type location()      :: {file:filename(), Line :: pos_integer()}.
-type bag(Key, Value) :: #{Key => sets:set(Value)}.
-type result(Value)   :: {ok, Value} | {error, term()}.

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
  Append = fun (Key, Set, Bag) -> sets:union(Set, maps:get(Key, Bag, sets:new())) end,
  maps:fold(Append, Bag2, Bag1).

-spec display_atoms(bag(atom(), location())) -> ok.
display_atoms(Atoms) ->
  lists:foreach(fun (Atom) -> display_atom(Atom, maps:get(Atom, Atoms)) end,
                lists:sort(maps:keys(Atoms))).

-spec display_atom(atom(), sets:set(location())) -> ok.
display_atom(Atom, Locations) ->
  io:format("~p~n", [Atom]),
  lists:foreach(fun display_location/1, lists:sort(sets:to_list(Locations))),
  io:format("~n").

-spec display_location(location()) -> ok.
display_location({F, L}) -> io:format("~s:~p~n", [filename:absname(F), L]).

-spec atomize_files([file:filename()]) -> result(bag(atom(), location())).
atomize_files(FileNames) ->
  lists:foldl(fun (FileName, {ok, Bag}) ->
                    case atomize_file(FileName) of
                      {ok, Bag1} -> {ok, append_bags(Bag, Bag1)};
                      Error -> Error
                    end;
                  (_, Error) -> Error
              end, {ok, #{}}, FileNames).

-spec atomize_file(file:filename()) -> result(bag(atom(), location())).
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

-spec to_bag([{Key, Value}]) -> bag(Key, Value) when
  Key   :: term(),
  Value :: term().
to_bag(List) ->
  lists:foldl(fun ({Key, Value}, Bag) ->
                Set = maps:get(Key, Bag, sets:new()),
                maps:put(Key, sets:add_element(Value, Set), Bag)
              end, #{}, List).

-spec atomize_form (erl_parse:abstract_form()) -> atoms();
                   (erl_parse:form_info())     -> [].
atomize_form(Form) -> case Form of
  {attribute, _, module,      _} -> [];
  {attribute, _, behavior,    _} -> [];
  {attribute, _, behaviour,   _} -> [];
  {attribute, _, export,      _} -> [];
  {attribute, _, import,      _} -> [];
  {attribute, _, export_type, _} -> [];
  {attribute, _, compile,     _} -> [];
  {attribute, _, file,        _} -> [];
  {attribute, _, record,   {_, Fs  }} -> lists:flatmap(fun atomize_record_field_decl/1, Fs);
  {attribute, _, type,     {_, T, _}} -> atomize_type(T);
  {attribute, _, opaque,   {_, T, _}} -> atomize_type(T);
  {attribute, _, callback, {_, Ts  }} -> lists:flatmap(fun atomize_function_type/1, Ts);
  {attribute, _, spec,     {_, Ts  }} -> lists:flatmap(fun atomize_function_type/1, Ts);
  {attribute, _, _,                _} -> []; %% TODO: Should we parse custom module attributes?
  {function,  _, _, _, Cs} -> lists:flatmap(fun atomize_clause/1, Cs);
  {eof, _} -> [];
  {error,   Info} -> io:format("Error: ~w~n",   [Info]), [];
  {warning, Info} -> io:format("Warning: ~w~n", [Info]), []
end.

-spec atomize_expr(erl_parse:abstract_expr()) -> atoms().
atomize_expr(Expr) -> case Expr of
  {atom,      L,        A} -> [{A, L}];
  {char,      _,        _} -> [];
  {float,     _,        _} -> [];
  {integer,   _,        _} -> [];
  {string,    _,        _} -> [];
  {match,     _,     P, E} -> atomize_pattern(P) ++ atomize_expr(E);
  {var,       _,        _} -> [];
  {tuple,     _,       Es} -> lists:flatmap(fun atomize_expr/1, Es);
  {nil,       _          } -> [];
  {cons,      _,     H, T} -> atomize_expr(H) ++ atomize_expr(T);
  {bin,       _,      BEs} -> lists:flatmap(fun atomize_bin_element/1, BEs);
  {op,        _, _,  A, B} -> atomize_expr(A) ++ atomize_expr(B);
  {op,        _, _,     E} -> atomize_expr(E);
  {record,    _,    _, Fs} -> lists:flatmap(fun atomize_record_field/1, Fs);
  {record,    _, E, _, Fs} -> atomize_expr(E) ++ lists:flatmap(fun atomize_record_field/1, Fs);
  {remote,    _, {atom, _, _}, {atom, _, _}} -> [];
  {remote,    _, A, {atom, _, _}} -> atomize_expr(A);
  {remote,    _, {atom, _, _}, B} -> atomize_expr(B);
  {remote,    _,     A, B} -> atomize_expr(A) ++ atomize_expr(B);
  {record_index, _,    _,  _} -> [];
  {record_field, _, E, _,  _} -> atomize_expr(E);
  {map,       _,       As} -> lists:flatmap(fun atomize_assoc/1, As);
  {map,       _, E,    As} -> atomize_expr(E) ++ lists:flatmap(fun atomize_assoc/1, As);
  {'catch',   _, E       } -> atomize_expr(E);
  {call,      _, {atom, _, _}, Es} -> lists:flatmap(fun atomize_expr/1, Es);
  {call,      _, E,    Es} -> atomize_expr(E) ++ lists:flatmap(fun atomize_expr/1, Es);
  {lc,        _,    T, Qs} -> atomize_expr(T) ++ lists:flatmap(fun atomize_qualifier/1, Qs);
  {bc,        _,    T, Qs} -> atomize_expr(T) ++ lists:flatmap(fun atomize_qualifier/1, Qs);
  {block,     _,       Es} -> lists:flatmap(fun atomize_expr/1, Es);
  {'if',      _,       Cs} -> lists:flatmap(fun atomize_clause/1, Cs);
  {'case',    _,    E, Cs} -> atomize_expr(E) ++ lists:flatmap(fun atomize_clause/1, Cs);
  {'try',     _, Es, Scs, Ccs, As} -> lists:flatmap(fun atomize_expr/1, Es ++ As)
                                   ++ lists:flatmap(fun atomize_clause/1, Scs ++ Ccs);
  {'receive', _, Cs       } -> lists:flatmap(fun atomize_clause/1, Cs);
  {'receive', _, Cs, E, Es} -> lists:flatmap(fun atomize_clause/1, Cs)
                           ++ atomize_expr(E)
                           ++ lists:flatmap(fun atomize_expr/1, Es);
  {'fun',     _, {function, _, _}}    -> [];
  {'fun',     _, {function, _, _, _}} -> [];
  {'fun',     _, {clauses, Cs}} -> lists:flatmap(fun atomize_clause/1, Cs);
  {named_fun, _, _, Cs} -> lists:flatmap(fun atomize_clause/1, Cs)
end.

-spec atomize_bin_element(BinElement) -> atoms() when
  BinElement :: erl_parse:af_binelement(erl_parse:abstract_expr()).
atomize_bin_element(BinElement) -> case BinElement of
  {bin_element, _, E, default, _} -> atomize_expr(E);
  {bin_element, _, E, S,       _} -> atomize_expr(E) ++ atomize_expr(S)
end.

-spec atomize_bin_element_pattern(BinElement) -> atoms() when
  BinElement :: erl_parse:af_binelement(erl_parse:af_pattern()).
atomize_bin_element_pattern(BinElement) -> case BinElement of
  {bin_element, _, E, default, _} -> atomize_pattern(E);
  {bin_element, _, E, S,       _} -> atomize_pattern(E) ++ atomize_pattern(S)
end.

-spec atomize_record_field_decl(erl_parse:af_field_decl()) -> atoms().
atomize_record_field_decl(FieldDecl) -> case FieldDecl of
  {typed_record_field, F, T} -> atomize_record_field(F) ++ atomize_type(T);
  F -> atomize_record_field(F)
end.

-spec atomize_record_field(erl_parse:af_field()) -> atoms().
atomize_record_field(Field) -> case Field of
  {record_field, _, _}    -> [];
  {record_field, _, _, E} -> atomize_expr(E)
end.

-spec atomize_clause(erl_parse:af_clause()) -> atoms().
atomize_clause({clause, _, Ps, Gs, Es}) ->
  lists:flatmap(fun atomize_pattern/1, Ps) ++
  lists:flatmap(fun atomize_guard/1, lists:flatten(Gs)) ++
  lists:flatmap(fun atomize_expr/1, Es).

-spec atomize_pattern(erl_parse:af_pattern()) -> atoms().
atomize_pattern(Pattern) -> case Pattern of
  {atom,    L,         A} -> [{A, L}];
  {char,    _,         _} -> [];
  {float,   _,         _} -> [];
  {integer, _,         _} -> [];
  {string,  _,         _} -> [];
  {match,   _,    P1, P2} -> atomize_pattern(P1) ++ atomize_pattern(P2);
  {var,     _,         _} -> [];
  {tuple,   _,        Ps} -> lists:flatmap(fun atomize_pattern/1, Ps);
  {nil,     _           } -> [];
  {cons,    _,    P1, P2} -> atomize_pattern(P1) ++ atomize_pattern(P2);
  {bin,     _,        Bs} -> lists:flatmap(fun atomize_bin_element_pattern/1, Bs);
  {op,      _, _, P1, P2} -> atomize_pattern(P1) ++ atomize_pattern(P2);
  {op,      _, _,      P} -> atomize_pattern(P);
  {map,     _,        As} -> lists:flatmap(fun atomize_assoc/1, As);
  {record,  _,     _, Fs} -> lists:flatmap(fun atomize_record_field/1, Fs);
  {record_index, _, _, _} -> []
end.

-spec atomize_guard(erl_parse:af_guard_test()) -> atoms().
atomize_guard(Guard) -> case Guard of
  {atom,    L,         A} -> [{A, L}];
  {char,    _,         _} -> [];
  {float,   _,         _} -> [];
  {integer, _,         _} -> [];
  {string,  _,         _} -> [];
  {var,     _,         _} -> [];
  {tuple,   _,        Gs} -> lists:flatmap(fun atomize_guard/1, Gs);
  {nil,     _           } -> [];
  {cons,    _,    G1, G2} -> atomize_guard(G1) ++ atomize_guard(G2);
  {bin,     _,       BEs} -> lists:flatmap(fun atomize_bin_element/1, BEs);
  {op,      _, _, G1, G2} -> atomize_guard(G1) ++ atomize_guard(G2);
  {op,      _, _,      G} -> atomize_guard(G);
  {record,  _,     _, Fs} -> lists:flatmap(fun atomize_record_field_guard/1, Fs);
  {map,     _,        As} -> lists:flatmap(fun atomize_assoc/1, As);
  {map,     _,     G, As} -> atomize_guard(G) ++ lists:flatmap(fun atomize_assoc/1, As);
  {call,    _,     _, Gs} -> lists:flatmap(fun atomize_guard/1, Gs);
  {record_index, _,    _, _} -> [];
  {record_field, _, G, _, _} -> atomize_guard(G)
end.

-spec atomize_record_field_guard(RecordField) -> atoms() when
  RecordField :: erl_parse:af_record_field(erl_parse:af_guard_test()).
atomize_record_field_guard({record_field, _, _, G}) -> atomize_guard(G).

-spec atomize_assoc(erl_parse:af_assoc(erl_parse:abstract_expr())) -> atoms().
atomize_assoc(Assoc) -> case Assoc of
  {map_field_assoc, _, K, V} -> atomize_expr(K) ++ atomize_expr(V);
  {map_field_exact, _, K, V} -> atomize_expr(K) ++ atomize_expr(V)
end.

-spec atomize_qualifier(erl_parse:af_qualifier()) -> atoms().
atomize_qualifier(Qualifier) -> case Qualifier of
  {generate,   _, P, E} -> atomize_pattern(P) ++ atomize_expr(E);
  {b_generate, _, P, E} -> atomize_pattern(P) ++ atomize_expr(E);
  E -> atomize_expr(E)
end.

-spec atomize_type(erl_parse:abstract_type()) -> atoms().
atomize_type(Type) -> case Type of
  {type, _, any}                 -> [];
  {type, _, binary,           _} -> [];
  {type, _, nil,              _} -> [];
  {type, _, 'fun',           Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, product,         Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, range,            _} -> [];
  {type, _, map,            any} -> [];
  {type, _, map_field_assoc, Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, map_field_exact, Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, record,           _} -> [];
  {type, _, field_type,       _} -> [];
  {type, _, tuple,          any} -> [];
  {type, _, tuple,           Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, union,           Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {type, _, _,               Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {ann_type,    _,           Ts} -> lists:flatmap(fun atomize_type/1, Ts);
  {remote_type, _,            _} -> [];
  {atom,        L,            A} -> [{A, L}];
  {char,        _,            _} -> [];
  {integer,     _,            _} -> [];
  {op,          _,         _, _} -> [];
  {op,          _,      _, _, _} -> [];
  {var,         _,            _} -> [];
  {user_type,   _,        _, Ts} -> lists:flatmap(fun atomize_type/1, Ts)
end.

-spec atomize_function_type(FunctionType) -> atoms() when
  FunctionType :: erl_parse:af_function_type()
                | erl_parse:af_constrained_function_type().
atomize_function_type(FunctionType) -> case FunctionType of
  {type, _, 'fun', Ps} -> lists:flatmap(fun atomize_type/1, Ps);
  {type, _, bounded_fun, [T, Cs]} ->
    atomize_function_type(T) ++ lists:flatmap(fun atomize_constraint/1, Cs)
end.

-spec atomize_constraint(erl_parse:af_constraint()) -> atoms().
atomize_constraint({type, _, constraint, [{atom, _, is_subtype}, Ts]}) ->
  lists:flatmap(fun atomize_type/1, Ts).
