-export_type([
    source/0,
    atoms/0,
    location/0,
    position/0,
    warning/0,
    warnings/0,
    path/0,
    normal_form/0
]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type position() :: pos_integer().

-type path() :: {erl, file:filename()}
              | {dir, file:filename()}.

-type location()  :: {file:filename(), position()}.
-type locations() :: #{file:filename() => sets:set(position())}.
-type atoms()     :: #{atom() => locations()}.
-type warning()   :: {atom(), atom()}.
-type warnings()  :: sets:set(warning()).

-type normal_form() :: nonempty_list(atom()).
