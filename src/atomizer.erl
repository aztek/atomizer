-module(atomizer).

-export_type([location/0, result/1]).

-type location()    :: {file:filename(), Line :: pos_integer()}.
-type result(Value) :: {ok, Value} | {error, term()}.

-export([
  atomize_files/1,
  atomize_file/2
]).

-spec atomize_files([file:filename()]) -> result(bags:bag(atom(), location())).
atomize_files(FileNames) ->
  Pid = self(),
  lists:foreach(fun (FileName) ->
                  spawn_link(?MODULE, atomize_file, [FileName, Pid])
                end, FileNames),
  atomize_loop(sets:from_list(FileNames), bags:empty()).

atomize_loop(FileNames, Atoms) ->
  case sets:is_empty(FileNames) of
    true  -> {ok, Atoms};
    false ->
      receive
        {atom, A, F, L}  -> atomize_loop(FileNames, bags:put(A, {F, L}, Atoms));
        {done, FileName} -> atomize_loop(sets:del_element(FileName, FileNames), Atoms);
        {error, Error}   -> {error, Error}
      end
  end.

-spec atomize_file(file:filename(), pid()) -> ok.
atomize_file(FileName, Callback) ->
  put(filename, FileName),
  put(callback, Callback),
  Callback ! case epp:parse_file(FileName, []) of
    {ok, Forms}    -> lists:foreach(fun atomize_form/1, Forms), {done, FileName};
    {ok, Forms, _} -> lists:foreach(fun atomize_form/1, Forms), {done, FileName};
    {error, Error} -> {error, Error}
  end,
  ok.

-spec atomize_form(erl_parse:abstract_form() | erl_parse:form_info()) -> ok.
atomize_form(Form) -> case Form of
  {attribute, _, module,      _} -> ok;
  {attribute, _, behavior,    _} -> ok;
  {attribute, _, behaviour,   _} -> ok;
  {attribute, _, export,      _} -> ok;
  {attribute, _, import,      _} -> ok;
  {attribute, _, export_type, _} -> ok;
  {attribute, _, compile,     _} -> ok;
  {attribute, _, file,        _} -> ok;
  {attribute, _, record,   {_, Fs  }} -> lists:foreach(fun atomize_record_field_decl/1, Fs);
  {attribute, _, type,     {_, T, _}} -> atomize_type(T);
  {attribute, _, opaque,   {_, T, _}} -> atomize_type(T);
  {attribute, _, callback, {_, Ts  }} -> lists:foreach(fun atomize_function_type/1, Ts);
  {attribute, _, spec,     {_, Ts  }} -> lists:foreach(fun atomize_function_type/1, Ts);
  {attribute, _, _,                _} -> ok; %% TODO: Should we parse custom module attributes?
  {function,  _, _, _, Cs} -> lists:foreach(fun atomize_clause/1, Cs);
  {eof, _} -> ok;
  {error,   Info} -> io:format("Error: ~w~n",   [Info]), [];
  {warning, Info} -> io:format("Warning: ~w~n", [Info]), []
end.

-spec atomize_expr(erl_parse:abstract_expr()) -> ok.
atomize_expr(Expr) -> case Expr of
  {atom,      L,        A} -> get(callback) ! {atom, A, get(filename), L}, ok;
  {char,      _,        _} -> ok;
  {float,     _,        _} -> ok;
  {integer,   _,        _} -> ok;
  {string,    _,        _} -> ok;
  {match,     _,     P, E} -> atomize_pattern(P), atomize_expr(E);
  {var,       _,        _} -> ok;
  {tuple,     _,       Es} -> lists:foreach(fun atomize_expr/1, Es);
  {nil,       _          } -> ok;
  {cons,      _,     H, T} -> atomize_expr(H), atomize_expr(T);
  {bin,       _,      BEs} -> lists:foreach(fun atomize_bin_element/1, BEs);
  {op,        _, _,  A, B} -> atomize_expr(A), atomize_expr(B);
  {op,        _, _,     E} -> atomize_expr(E);
  {record,    _,    _, Fs} -> lists:foreach(fun atomize_record_field/1, Fs);
  {record,    _, E, _, Fs} -> atomize_expr(E), lists:foreach(fun atomize_record_field/1, Fs);
  {remote,    _, {atom, _, _}, {atom, _, _}} -> ok;
  {remote,    _, A, {atom, _, _}} -> atomize_expr(A);
  {remote,    _, {atom, _, _}, B} -> atomize_expr(B);
  {remote,    _,     A, B} -> atomize_expr(A), atomize_expr(B);
  {record_index, _,    _,  _} -> ok;
  {record_field, _, E, _,  _} -> atomize_expr(E);
  {map,       _,       As} -> lists:foreach(fun atomize_assoc/1, As);
  {map,       _, E,    As} -> atomize_expr(E), lists:foreach(fun atomize_assoc/1, As);
  {'catch',   _, E       } -> atomize_expr(E);
  {call,      _, {atom, _, _}, Es} -> lists:foreach(fun atomize_expr/1, Es);
  {call,      _, E,    Es} -> lists:foreach(fun atomize_expr/1, [E | Es]);
  {lc,        _,    T, Qs} -> atomize_expr(T), lists:foreach(fun atomize_qualifier/1, Qs);
  {bc,        _,    T, Qs} -> atomize_expr(T), lists:foreach(fun atomize_qualifier/1, Qs);
  {block,     _,       Es} -> lists:foreach(fun atomize_expr/1, Es);
  {'if',      _,       Cs} -> lists:foreach(fun atomize_clause/1, Cs);
  {'case',    _,    E, Cs} -> atomize_expr(E), lists:foreach(fun atomize_clause/1, Cs);
  {'try',     _, Es, Scs, Ccs, As} -> lists:foreach(fun atomize_expr/1, Es ++ As),
                                      lists:foreach(fun atomize_clause/1, Scs ++ Ccs);
  {'receive', _, Cs       } -> lists:foreach(fun atomize_clause/1, Cs);
  {'receive', _, Cs, E, Es} -> lists:foreach(fun atomize_clause/1, Cs),
                               atomize_expr(E),
                               lists:foreach(fun atomize_expr/1, Es);
  {'fun',     _, {function, _, _}}    -> ok;
  {'fun',     _, {function, _, _, _}} -> ok;
  {'fun',     _, {clauses, Cs}} -> lists:foreach(fun atomize_clause/1, Cs);
  {named_fun, _, _, Cs} -> lists:foreach(fun atomize_clause/1, Cs)
end.

-spec atomize_bin_element(BinElement) -> ok when
  BinElement :: erl_parse:af_binelement(erl_parse:abstract_expr()).
atomize_bin_element(BinElement) -> case BinElement of
  {bin_element, _, E, default, _} -> atomize_expr(E);
  {bin_element, _, E, S,       _} -> atomize_expr(E), atomize_expr(S)
end.

-spec atomize_bin_element_pattern(BinElement) -> ok when
  BinElement :: erl_parse:af_binelement(erl_parse:af_pattern()).
atomize_bin_element_pattern(BinElement) -> case BinElement of
  {bin_element, _, E, default, _} -> atomize_pattern(E);
  {bin_element, _, E, S,       _} -> atomize_pattern(E), atomize_pattern(S)
end.

-spec atomize_record_field_decl(erl_parse:af_field_decl()) -> ok.
atomize_record_field_decl(FieldDecl) -> case FieldDecl of
  {typed_record_field, F, T} -> atomize_record_field(F), atomize_type(T);
  F -> atomize_record_field(F)
end.

-spec atomize_record_field(erl_parse:af_field()) -> ok.
atomize_record_field(Field) -> case Field of
  {record_field, _, _}    -> ok;
  {record_field, _, _, E} -> atomize_expr(E)
end.

-spec atomize_clause(erl_parse:af_clause()) -> ok.
atomize_clause({clause, _, Ps, Gs, Es}) ->
  lists:foreach(fun atomize_pattern/1, Ps),
  lists:foreach(fun atomize_guard/1, lists:flatten(Gs)),
  lists:foreach(fun atomize_expr/1, Es).

-spec atomize_pattern(erl_parse:af_pattern()) -> ok.
atomize_pattern(Pattern) -> case Pattern of
  {atom,    L,         A} -> get(callback) ! {atom, A, get(filename), L}, ok;
  {char,    _,         _} -> ok;
  {float,   _,         _} -> ok;
  {integer, _,         _} -> ok;
  {string,  _,         _} -> ok;
  {match,   _,    P1, P2} -> atomize_pattern(P1), atomize_pattern(P2);
  {var,     _,         _} -> ok;
  {tuple,   _,        Ps} -> lists:foreach(fun atomize_pattern/1, Ps);
  {nil,     _           } -> ok;
  {cons,    _,    P1, P2} -> atomize_pattern(P1), atomize_pattern(P2);
  {bin,     _,        Bs} -> lists:foreach(fun atomize_bin_element_pattern/1, Bs);
  {op,      _, _, P1, P2} -> atomize_pattern(P1), atomize_pattern(P2);
  {op,      _, _,      P} -> atomize_pattern(P);
  {map,     _,        As} -> lists:foreach(fun atomize_assoc/1, As);
  {record,  _,     _, Fs} -> lists:foreach(fun atomize_record_field/1, Fs);
  {record_index, _, _, _} -> ok
end.

-spec atomize_guard(erl_parse:af_guard_test()) -> ok.
atomize_guard(Guard) -> case Guard of
  {atom,    L,         A} -> get(callback) ! {atom, A, get(filename), L}, ok;
  {char,    _,         _} -> ok;
  {float,   _,         _} -> ok;
  {integer, _,         _} -> ok;
  {string,  _,         _} -> ok;
  {var,     _,         _} -> ok;
  {tuple,   _,        Gs} -> lists:foreach(fun atomize_guard/1, Gs);
  {nil,     _           } -> ok;
  {cons,    _,    G1, G2} -> atomize_guard(G1), atomize_guard(G2);
  {bin,     _,       BEs} -> lists:foreach(fun atomize_bin_element/1, BEs);
  {op,      _, _, G1, G2} -> atomize_guard(G1), atomize_guard(G2);
  {op,      _, _,      G} -> atomize_guard(G);
  {record,  _,     _, Fs} -> lists:foreach(fun atomize_record_field_guard/1, Fs);
  {map,     _,        As} -> lists:foreach(fun atomize_assoc/1, As);
  {map,     _,     G, As} -> atomize_guard(G), lists:foreach(fun atomize_assoc/1, As);
  {call,    _,     _, Gs} -> lists:foreach(fun atomize_guard/1, Gs);
  {record_index, _,    _, _} -> ok;
  {record_field, _, G, _, _} -> atomize_guard(G)
end.

-spec atomize_record_field_guard(RecordField) -> ok when
  RecordField :: erl_parse:af_record_field(erl_parse:af_guard_test()).
atomize_record_field_guard({record_field, _, _, G}) -> atomize_guard(G).

-spec atomize_assoc(erl_parse:af_assoc(erl_parse:abstract_expr())) -> ok.
atomize_assoc(Assoc) -> case Assoc of
  {map_field_assoc, _, K, V} -> atomize_expr(K), atomize_expr(V);
  {map_field_exact, _, K, V} -> atomize_expr(K), atomize_expr(V)
end.

-spec atomize_qualifier(erl_parse:af_qualifier()) -> ok.
atomize_qualifier(Qualifier) -> case Qualifier of
  {generate,   _, P, E} -> atomize_pattern(P), atomize_expr(E);
  {b_generate, _, P, E} -> atomize_pattern(P), atomize_expr(E);
  E -> atomize_expr(E)
end.

-spec atomize_type(erl_parse:abstract_type()) -> ok.
atomize_type(Type) -> case Type of
  {type, _, any}           -> ok;
  {type, _, binary,     _} -> ok;
  {type, _, nil,        _} -> ok;
  {type, _, 'fun',     Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, product,   Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, range,      _} -> ok;
  {type, _, map,      any} -> ok;
  {type, _, map_field_assoc, Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, map_field_exact, Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, record,     _} -> ok;
  {type, _, field_type, _} -> ok;
  {type, _, tuple,    any} -> ok;
  {type, _, tuple,     Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, union,     Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {type, _, _,         Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {ann_type,    _,     Ts} -> lists:foreach(fun atomize_type/1, Ts);
  {remote_type, _,      _} -> ok;
  {atom,        L,      A} -> get(callback) ! {atom, A, get(filename), L};
  {char,        _,      _} -> ok;
  {integer,     _,      _} -> ok;
  {op,          _,   _, _} -> ok;
  {op,          _, _, _, _} -> ok;
  {var,         _,       _} -> ok;
  {user_type,   _,   _, Ts} -> lists:foreach(fun atomize_type/1, Ts)
end.

-spec atomize_function_type(FunctionType) -> ok when
  FunctionType :: erl_parse:af_function_type()
                | erl_parse:af_constrained_function_type().
atomize_function_type(FunctionType) -> case FunctionType of
  {type, _, 'fun', Ps} -> lists:foreach(fun atomize_type/1, Ps);
  {type, _, bounded_fun, [T, Cs]} ->
    atomize_function_type(T), lists:foreach(fun atomize_constraint/1, Cs)
end.

-spec atomize_constraint(erl_parse:af_constraint()) -> ok.
atomize_constraint({type, _, constraint, [{atom, _, is_subtype}, Ts]}) ->
  lists:foreach(fun atomize_type/1, Ts).
