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

-type source() :: {erl, file:filename()}
                | {dir, file:filename()}
                | {symlink, file:filename()}.

-type location()  :: {file:filename(), position()}.
-type locations() :: #{file:filename() => sets:set(position())}.
-type atoms()     :: #{atom() => locations()}.
-type warning()   :: {atom(), atom()}.
-type warnings()  :: sets:set(warning()).

-type normal_form() :: atom().
