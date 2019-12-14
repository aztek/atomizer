-export_type([source/0, position/0, path/0]).

-type source() :: {file, file:filename()}
                | {dir,  file:filename()}
                | {files, [file:filename()]}
                | {dirs,  [file:filename()]}.

-type position() :: pos_integer().

-type path() :: {erl, file:filename()}
              | {dir, file:filename()}.
