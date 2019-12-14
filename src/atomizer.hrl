-export_type([source/0, atoms/0, location/0, position/0, warnings/0, path/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type position() :: pos_integer().

-type path() :: {erl, file:filename()}
              | {dir, file:filename()}.

-type location()  :: {file:filename(), position()}.
-type locations() :: multimaps:multimap(file:filename(), position()).
-type atoms()     :: #{atom() => locations()}.
-type warnings()  :: sets:set({atom(), atom()}).

