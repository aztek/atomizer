-module(atomizer).

-export([
    cli_init/0,
    cli_set/2,
    cli_get/2,
    cli_set_warn_errors/1,
    cli_get_warn_errors/0,
    cli_set_verbosity/1,
    cli_get_verbosity/0,

    nr_files/1,
    nr_occurrences/1,

    print/1,
    print/2,
    error/1,
    error/2,
    warning/1,
    warning/2,

    plural/3
]).

-export_type([
    source/0,
    atoms/0,
    location/0,
    locations/0,
    position/0,
    warning/0,
    warnings/0,
    normal_form/0
]).

-type source() :: {erl,  file:filename()}
                | {beam, file:filename()}
                | {dir,  file:filename()}.

-type atoms()     :: #{atom() => locations()}.
-type warning()   :: {atom(), atom()}.
-type warnings()  :: sets:set(warning()).

-type normal_form() :: atom().


%%% Global dictionary of command line arguments populated at startup

-define(CLI_TABLE, cli).
-define(CLI_WARN_ERRORS, warn_errors).
-define(CLI_VERBOSITY, verbosity).

cli_init() ->
    ets:new(?CLI_TABLE, [set, protected, named_table]).

cli_set(Param, Value) ->
    ets:insert(?CLI_TABLE, {Param, Value}).

cli_get(Param, Default) ->
    case ets:lookup(?CLI_TABLE, Param) of
        [{Param, Value}] -> Value;
        _ -> Default
    end.

cli_set_warn_errors(Value) -> cli_set(?CLI_WARN_ERRORS, Value).
cli_get_warn_errors() -> cli_get(?CLI_WARN_ERRORS, false).

cli_set_verbosity(Value) -> cli_set(?CLI_VERBOSITY, Value).
cli_get_verbosity() -> cli_get(?CLI_VERBOSITY, 2).


%%% Locations

-type line()      :: non_neg_integer().
-type column()    :: non_neg_integer().
-type position()  :: line() | {line(), column()}.
-type location()  :: {file:filename(), position()}.
-type locations() :: #{file:filename() => sets:set(position())}.

-spec nr_files(atomizer:locations()) -> non_neg_integer().
nr_files(Locations) -> maps:size(Locations).

-spec nr_occurrences(atomizer:locations()) -> non_neg_integer().
nr_occurrences(Locations) ->
    maps:fold(fun (_, V, S) -> sets:size(V) + S end, 0, Locations).


%%% Printing to standard outputs

print(Message) ->
    atomizer_output:put_chars(standard_io, [Message, "\n"]).

print(Format, Args) ->
    ?MODULE:print(io_lib:format(Format, Args)).

-spec error(io_lib:chars()) -> ok.
error(Error) ->
    atomizer_output:put_chars(standard_error, ["\e[31mError: ", Error, "\e[00m\n"]).

-spec error(string(), [term()]) -> ok.
error(Format, Args) ->
    ?MODULE:error(io_lib:format(Format, Args)).

-spec warning(io_lib:chars()) -> ok.
warning(Warning) ->
    case cli_get_verbosity() of
        0 -> ok;
        _ -> atomizer_output:put_chars(standard_error, ["\e[33mWarning: ", Warning, "\e[00m\n"])
    end.

-spec warning(string(), [term()]) -> ok.
warning(Format, Args) ->
    warning(io_lib:format(Format, Args)).


%%% Miscellaneous

-spec plural(non_neg_integer(), string(), string()) -> string().
plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.
