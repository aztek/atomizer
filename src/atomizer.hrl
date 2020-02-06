-export_type([
    source/0,
    atoms/0,
    location/0,
    position/0,
    warning/0,
    warnings/0,
    normal_form/0
]).

-type position() :: pos_integer().

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

-define(ERRORS_AS_WARNINGS,
        case ets:lookup(?CLI_OPTIONS_TABLE, warn_errors) of
            [{warn_errors, WarnErrors}] -> WarnErrors;
            _ -> false
        end).

-define(ERROR(Error),
    io:put_chars(standard_error, ["\e[31mError: ", Error, "\e[00m\n"])).

-define(ERROR(Format, Args),
    ?ERROR(io_lib:format(Format, Args))).

-define(WARNING(Warning),
    io:put_chars(standard_error, ["\e[33mWarning: ", Warning, "\e[00m\n"])).

-define(WARNING(Format, Args),
    ?WARNING(io_lib:format(Format, Args))).
