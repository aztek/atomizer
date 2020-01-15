-module(atomizer_parse).

-include("atomizer.hrl").

-include_lib("kernel/include/file.hrl").

-define(ERLANG_EXTENSIONS, [".erl", ".hrl"]).

-export([
    parse_source/2,
    detect_source/1
]).

-spec parse_path(pid(), file:filename()) -> any().
parse_path(Pid, Path) ->
    case detect_source(Path) of
        {ok, other}    -> ignore;
        {ok, Source}   -> Pid ! {add_source, Source};
        {error, Error} -> Pid ! {error, Error}
    end.

-spec parse_paths(pid(), [file:filename()]) -> any().
parse_paths(Pid, Paths) ->
    lists:foreach(fun (Path) -> parse_path(Pid, Path) end, Paths).

-spec detect_source(file:filename()) -> {ok, source() | other} | {error, term()}.
detect_source(Path) ->
    case file:read_file_info(Path) of
        {ok, Info} ->
            Type = case {Info#file_info.type, is_erlang(Path)} of
                {directory, _}  -> {dir, Path};
                {regular, true} -> {erl, Path};
                _ -> other
            end,
            {ok, Type};

        {error, enoent} ->
            % Error because Path is a symlink?
            case file:read_link_all(Path) of
                {ok, _}    -> {ok, {symlink, Path}};
                {error, _} -> {error, {enoent, Path}}
            end;

        {error, Error} ->
            {error, {Error, Path}}
    end.

-spec is_erlang(file:filename()) -> boolean().
is_erlang(FileName) ->
    lists:any(fun (Ext) -> lists:suffix(Ext, FileName) end, ?ERLANG_EXTENSIONS).

-spec parse_source(pid(), source()) -> any().
parse_source(Pid, Source = {erl, File}) ->
    put(filename, File),
    put(pid, Pid),
    case epp:open(File, []) of
        {ok, Epp} ->
            parse_epp(Epp),
            Pid ! {done_source, Source};

        {error, Error} ->
            Pid ! {error, {Error, File}}
    end;

parse_source(Pid, Source = {dir, Dir}) ->
    case file:list_dir(Dir) of
        {ok, Names} ->
            parse_paths(Pid, [filename:join(Dir, Name) || Name <- Names]),
            Pid ! {done_source, Source};

        {error, Error} ->
            Pid ! {error, {Error, Dir}}
    end;

parse_source(Pid, Source = {symlink, Path}) ->
    io:format(standard_error,
              "\e[33mWarning: Skipping symbolic link ~s\e[00m~n",
              [Path]),
    Pid ! {done_source, Source}.

-spec parse_epp(epp:epp_handle()) -> ok.
parse_epp(Epp) ->
    case epp:parse_erl_form(Epp) of
        {ok, Form} ->
            parse_form(Form),
            parse_epp(Epp);

        {eof, _} -> ok;

        {warning, {Line, Module, Warning}} ->
            io:format(standard_error,
                      "\e[33mWarning: Skipping ~s, unable to parse line ~p: ~s\e[00m~n",
                      [get(filename), Line, Module:format_error(Warning)]),
            parse_epp(Epp);

        {error, {_, epp, {include, file, _}}} ->
            parse_epp(Epp);

        {error, {_, epp, {undefined, _, _}}} ->
            parse_epp(Epp);

        {error, {Line, Module, Error}} ->
            io:format(standard_error,
                      "\e[33mWarning: Skipping ~s, unable to parse line ~p: ~s\e[00m~n",
                      [get(filename), Line, Module:format_error(Error)]),
            parse_epp(Epp)
    end.

-spec parse_form(erl_parse:abstract_form()) -> ok.
parse_form(Form) -> case Form of
    {attribute, _, module,      _} -> ok;
    {attribute, _, behavior,    _} -> ok;
    {attribute, _, behaviour,   _} -> ok;
    {attribute, _, export,      _} -> ok;
    {attribute, _, import,      _} -> ok;
    {attribute, _, export_type, _} -> ok;
    {attribute, _, compile,     _} -> ok;
    {attribute, _, file,        _} -> ok;
    {attribute, _, record,   {_, Fs  }} -> lists:foreach(fun parse_record_field_decl/1, Fs);
    {attribute, _, type,     {_, T, _}} -> parse_type(T);
    {attribute, _, opaque,   {_, T, _}} -> parse_type(T);
    {attribute, _, callback, {_, Ts  }} -> lists:foreach(fun parse_function_type/1, Ts);
    {attribute, _, spec,     {_, Ts  }} -> lists:foreach(fun parse_function_type/1, Ts);
    {attribute, _, _,                _} -> ok; %% TODO: Should we parse custom module attributes?
    {function,  _, _, _, Cs} -> lists:foreach(fun parse_clause/1, Cs)
end.

-spec parse_expr(erl_parse:abstract_expr()) -> ok.
parse_expr(Expr) -> case Expr of
    {atom,      L,        A} -> get(pid) ! {atom, A, get(filename), L}, ok;
    {char,      _,        _} -> ok;
    {float,     _,        _} -> ok;
    {integer,   _,        _} -> ok;
    {string,    _,        _} -> ok;
    {match,     _,     P, E} -> parse_pattern(P), parse_expr(E);
    {var,       _,        _} -> ok;
    {tuple,     _,       Es} -> lists:foreach(fun parse_expr/1, Es);
    {nil,       _          } -> ok;
    {cons,      _,     H, T} -> parse_expr(H), parse_expr(T);
    {bin,       _,      BEs} -> lists:foreach(fun parse_bin_element/1, BEs);
    {op,        _, _,  A, B} -> parse_expr(A), parse_expr(B);
    {op,        _, _,     E} -> parse_expr(E);
    {record,    _,    _, Fs} -> lists:foreach(fun parse_record_field/1, Fs);
    {record,    _, E, _, Fs} -> parse_expr(E), lists:foreach(fun parse_record_field/1, Fs);
    {remote,    _, {atom, _, _}, {atom, _, _}} -> ok;
    {remote,    _, A, {atom, _, _}} -> parse_expr(A);
    {remote,    _, {atom, _, _}, B} -> parse_expr(B);
    {remote,    _,     A, B} -> parse_expr(A), parse_expr(B);
    {record_index, _,    _,  _} -> ok;
    {record_field, _, E, _,  _} -> parse_expr(E);
    {map,       _,       As} -> lists:foreach(fun parse_assoc/1, As);
    {map,       _, E,    As} -> parse_expr(E), lists:foreach(fun parse_assoc/1, As);
    {'catch',   _, E       } -> parse_expr(E);
    {call,    _, {atom, _, is_record}, _} -> ok;
    {call,      _, {atom, _, _}, Es} -> lists:foreach(fun parse_expr/1, Es);
    {call,      _, E,    Es} -> lists:foreach(fun parse_expr/1, [E | Es]);
    {lc,        _,    T, Qs} -> parse_expr(T), lists:foreach(fun parse_qualifier/1, Qs);
    {bc,        _,    T, Qs} -> parse_expr(T), lists:foreach(fun parse_qualifier/1, Qs);
    {block,     _,       Es} -> lists:foreach(fun parse_expr/1, Es);
    {'if',      _,       Cs} -> lists:foreach(fun parse_clause/1, Cs);
    {'case',    _,    E, Cs} -> parse_expr(E), lists:foreach(fun parse_clause/1, Cs);
    {'try',     _, Es, Scs, Ccs, As} -> lists:foreach(fun parse_expr/1, Es ++ As),
                                        lists:foreach(fun parse_clause/1, Scs ++ Ccs);
    {'receive', _, Cs       } -> lists:foreach(fun parse_clause/1, Cs);
    {'receive', _, Cs, E, Es} -> lists:foreach(fun parse_clause/1, Cs),
                                 parse_expr(E),
                                 lists:foreach(fun parse_expr/1, Es);
    {'fun',     _, {function, _, _}}    -> ok;
    {'fun',     _, {function, _, _, _}} -> ok;
    {'fun',     _, {clauses, Cs}} -> lists:foreach(fun parse_clause/1, Cs);
    {named_fun, _, _, Cs} -> lists:foreach(fun parse_clause/1, Cs)
end.

-spec parse_bin_element(BinElement) -> ok when
    BinElement :: erl_parse:af_binelement(erl_parse:abstract_expr()).
parse_bin_element(BinElement) -> case BinElement of
    {bin_element, _, E, default, _} -> parse_expr(E);
    {bin_element, _, E, S,       _} -> parse_expr(E), parse_expr(S)
end.

-spec parse_bin_element_pattern(BinElement) -> ok when
    BinElement :: erl_parse:af_binelement(erl_parse:af_pattern()).
parse_bin_element_pattern(BinElement) -> case BinElement of
    {bin_element, _, E, default, _} -> parse_pattern(E);
    {bin_element, _, E, S,       _} -> parse_pattern(E), parse_pattern(S)
end.

-spec parse_record_field_decl(erl_parse:af_field_decl()) -> ok.
parse_record_field_decl(FieldDecl) -> case FieldDecl of
    {typed_record_field, F, T} -> parse_record_field(F), parse_type(T);
    F -> parse_record_field(F)
end.

-spec parse_record_field(erl_parse:af_field()) -> ok.
parse_record_field(Field) -> case Field of
    {record_field, _, _}    -> ok;
    {record_field, _, _, E} -> parse_expr(E)
end.

-spec parse_clause(erl_parse:af_clause()) -> ok.
parse_clause({clause, _, Ps, Gs, Es}) ->
    lists:foreach(fun parse_pattern/1, Ps),
    lists:foreach(fun parse_guard/1, lists:flatten(Gs)),
    lists:foreach(fun parse_expr/1, Es).

-spec parse_pattern(erl_parse:af_pattern()) -> ok.
parse_pattern(Pattern) -> case Pattern of
    {atom,    L,         A} -> get(pid) ! {atom, A, get(filename), L}, ok;
    {char,    _,         _} -> ok;
    {float,   _,         _} -> ok;
    {integer, _,         _} -> ok;
    {string,  _,         _} -> ok;
    {match,   _,    P1, P2} -> parse_pattern(P1), parse_pattern(P2);
    {var,     _,         _} -> ok;
    {tuple,   _,        Ps} -> lists:foreach(fun parse_pattern/1, Ps);
    {nil,     _           } -> ok;
    {cons,    _,    P1, P2} -> parse_pattern(P1), parse_pattern(P2);
    {bin,     _,        Bs} -> lists:foreach(fun parse_bin_element_pattern/1, Bs);
    {op,      _, _, P1, P2} -> parse_pattern(P1), parse_pattern(P2);
    {op,      _, _,      P} -> parse_pattern(P);
    {map,     _,        As} -> lists:foreach(fun parse_assoc/1, As);
    {record,  _,     _, Fs} -> lists:foreach(fun parse_record_field/1, Fs);
    {record_index, _, _, _} -> ok
end.

-spec parse_guard(erl_parse:af_guard_test()) -> ok.
parse_guard(Guard) -> case Guard of
    {atom,    L,         A} -> get(pid) ! {atom, A, get(filename), L}, ok;
    {char,    _,         _} -> ok;
    {float,   _,         _} -> ok;
    {integer, _,         _} -> ok;
    {string,  _,         _} -> ok;
    {var,     _,         _} -> ok;
    {tuple,   _,        Gs} -> lists:foreach(fun parse_guard/1, Gs);
    {nil,     _           } -> ok;
    {cons,    _,    G1, G2} -> parse_guard(G1), parse_guard(G2);
    {bin,     _,       BEs} -> lists:foreach(fun parse_bin_element/1, BEs);
    {op,      _, _, G1, G2} -> parse_guard(G1), parse_guard(G2);
    {op,      _, _,      G} -> parse_guard(G);
    {record,  _,     _, Fs} -> lists:foreach(fun parse_record_field_guard/1, Fs);
    {map,     _,        As} -> lists:foreach(fun parse_assoc/1, As);
    {map,     _,     G, As} -> parse_guard(G), lists:foreach(fun parse_assoc/1, As);
    {call,    _, {atom, _, is_record}, _} -> ok;
    {call,    _,     _, Gs} -> lists:foreach(fun parse_guard/1, Gs);
    {record_index, _,    _, _} -> ok;
    {record_field, _, G, _, _} -> parse_guard(G)
end.

-spec parse_record_field_guard(RecordField) -> ok when
    RecordField :: erl_parse:af_record_field(erl_parse:af_guard_test()).
parse_record_field_guard({record_field, _, _, G}) -> parse_guard(G).

-spec parse_assoc(erl_parse:af_assoc(erl_parse:abstract_expr())) -> ok.
parse_assoc(Assoc) -> case Assoc of
    {map_field_assoc, _, K, V} -> parse_expr(K), parse_expr(V);
    {map_field_exact, _, K, V} -> parse_expr(K), parse_expr(V)
end.

-spec parse_qualifier(erl_parse:af_qualifier()) -> ok.
parse_qualifier(Qualifier) -> case Qualifier of
    {generate,   _, P, E} -> parse_pattern(P), parse_expr(E);
    {b_generate, _, P, E} -> parse_pattern(P), parse_expr(E);
    E -> parse_expr(E)
end.

-spec parse_type(erl_parse:abstract_type()) -> ok.
parse_type(Type) -> case Type of
    {type, _, any}           -> ok;
    {type, _, binary,     _} -> ok;
    {type, _, nil,        _} -> ok;
    {type, _, 'fun',     Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, product,   Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, range,      _} -> ok;
    {type, _, map,      any} -> ok;
    {type, _, map_field_assoc, Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, map_field_exact, Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, record,     _} -> ok;
    {type, _, field_type, _} -> ok;
    {type, _, tuple,    any} -> ok;
    {type, _, tuple,     Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, union,     Ts} -> lists:foreach(fun parse_type/1, Ts);
    {type, _, _,         Ts} -> lists:foreach(fun parse_type/1, Ts);
    {ann_type,    _,     Ts} -> lists:foreach(fun parse_type/1, Ts);
    {remote_type, _,      _} -> ok;
    {atom,        L,      A} -> get(pid) ! {atom, A, get(filename), L};
    {char,        _,      _} -> ok;
    {integer,     _,      _} -> ok;
    {op,          _,   _, _} -> ok;
    {op,          _, _, _, _} -> ok;
    {var,         _,       _} -> ok;
    {user_type,   _,   _, Ts} -> lists:foreach(fun parse_type/1, Ts)
end.

-spec parse_function_type(FunctionType) -> ok when
    FunctionType :: erl_parse:af_function_type()
                  | erl_parse:af_constrained_function_type().
parse_function_type(FunctionType) -> case FunctionType of
    {type, _, 'fun', Ps} -> lists:foreach(fun parse_type/1, Ps);
    {type, _, bounded_fun, [T, Cs]} ->
        parse_function_type(T), lists:foreach(fun parse_constraint/1, Cs)
end.

-spec parse_constraint(erl_parse:af_constraint()) -> ok.
parse_constraint({type, _, constraint, [{atom, _, is_subtype}, Ts]}) ->
    lists:foreach(fun parse_type/1, Ts).
