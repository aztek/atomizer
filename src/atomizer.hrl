-export_type([
    source/0,
    atoms/0,
    location/0,
    position/0,
    warning/0,
    warnings/0,
    normal_form/0
]).

-type line()   :: non_neg_integer().
-type column() :: non_neg_integer().

-type position() :: line() | {line(), column()}.

-type source() :: {erl,  file:filename()}
                | {beam, file:filename()}
                | {dir,  file:filename()}.

-type location()  :: {file:filename(), position()}.
-type locations() :: #{file:filename() => sets:set(position())}.
-type atoms()     :: #{atom() => locations()}.
-type warning()   :: {atom(), atom()}.
-type warnings()  :: sets:set(warning()).

-type normal_form() :: atom().

-define(CLI_OPTIONS_TABLE, cli).
-define(CLI_OPTION(Name), element(2, hd(ets:lookup(?CLI_OPTIONS_TABLE, Name)))).

-define(WARN_ERRORS, ?CLI_OPTION(warn_errors)).
-define(VERBOSITY,   ?CLI_OPTION(verbosity)).

-define(ERROR(Error),
    io:put_chars(standard_error, ["\e[31mError: ", Error, "\e[00m\n"])).

-define(ERROR(Format, Args),
    ?ERROR(io_lib:format(Format, Args))).

-define(WARNING(Warning),
    case ?VERBOSITY of
        0 -> ok;
        _ -> io:put_chars(standard_error, ["\e[33mWarning: ", Warning, "\e[00m\n"])
    end).

-define(WARNING(Format, Args),
    ?WARNING(io_lib:format(Format, Args))).
