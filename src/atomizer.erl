-module(atomizer).

-export([
    package/5,
    package_paths/1,
    package_ignores/1,
    package_includes/1,
    package_follow_symlinks/1,
    package_parse_beams/1,

    nr_files/1,
    nr_occurrences/1,

    global_ignores/0,

    statistics/4,
    get_nr_loose_atoms/1,
    get_nr_atoms/1,
    get_nr_files/1,
    get_nr_dirs/1,

    format_error/1,

    print/1,
    nl/0,
    error/1,
    warning/1,

    pretty_atom/1,
    words/1,
    plural/3
]).

-export_type([
    file/0,
    source/0,
    atoms/0,
    location/0,
    locations/0,
    position/0,
    lookalikes/0,
    atom_info/0,
    loose_atom/0,
    normal_form/0,
    package/0,
    statistics/0,
    error/0
]).

-type file()        :: {erl | beam, file:filename()}.
-type source()      :: {dir, file:filename()} | file().

-type atoms()       :: #{atom() => locations()}.
-type lookalikes()  :: sets:set({atom(), atom()}).
-type normal_form() :: atom().

-type atom_info()   :: {atom(), locations()}.
-type loose_atom()  :: {LooseAtom :: atom_info(), Lookalike :: atom_info()}.


%%% Packages

-record(package, {
    paths           = []    :: [file:filename()],
    ignores         = []    :: [file:filename()],
    includes        = []    :: [file:filename()],
    follow_symlinks = false :: boolean(),
    parse_beams     = false :: boolean()
}).
-opaque package() :: #package{}.

-spec package([file:filename()], [file:filename()], [file:filename()], boolean(), boolean()) -> package().
package(Paths, Ignores, Includes, FollowSymlinks, ParseBeams) ->
    #package{
        paths           = Paths,
        ignores         = Ignores,
        includes        = Includes,
        follow_symlinks = FollowSymlinks,
        parse_beams     = ParseBeams
    }.

-spec package_paths(package()) -> [file:filename()].
package_paths(#package{paths = Paths}) -> Paths.

-spec package_ignores(package()) -> [file:filename()].
package_ignores(#package{ignores = Ignores}) -> Ignores.

-spec package_includes(package()) -> [file:filename()].
package_includes(#package{includes = Includes}) -> Includes.

-spec package_follow_symlinks(package()) -> boolean().
package_follow_symlinks(#package{follow_symlinks = FollowSymlinks}) -> FollowSymlinks.

-spec package_parse_beams(package()) -> boolean().
package_parse_beams(#package{parse_beams = ParseBeams}) -> ParseBeams.


%%% Statistics

-record(statistics, {
    nr_loose_atoms :: non_neg_integer() | -1,
    nr_atoms       :: non_neg_integer(),
    nr_files       :: non_neg_integer(),
    nr_dirs        :: non_neg_integer()
}).
-opaque statistics() :: #statistics{}.

-spec statistics(non_neg_integer() | -1, non_neg_integer(), non_neg_integer(), non_neg_integer()) -> statistics().
statistics(NrLooseAtoms, NrAtoms, NrFiles, NrDirs) ->
    #statistics{
        nr_loose_atoms = NrLooseAtoms,
        nr_atoms       = NrAtoms,
        nr_files       = NrFiles,
        nr_dirs        = NrDirs
    }.

-spec get_nr_loose_atoms(statistics()) -> non_neg_integer() | -1.
get_nr_loose_atoms(#statistics{nr_loose_atoms = NrLooseAtoms}) -> NrLooseAtoms.

-spec get_nr_atoms(statistics()) -> non_neg_integer().
get_nr_atoms(#statistics{nr_atoms = NrAtoms}) -> NrAtoms.

-spec get_nr_files(statistics()) -> non_neg_integer().
get_nr_files(#statistics{nr_files = NrFiles}) -> NrFiles.

-spec get_nr_dirs(statistics()) -> non_neg_integer().
get_nr_dirs(#statistics{nr_dirs = NrDirs}) -> NrDirs.


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

-spec global_ignores() -> [file:filename()].
global_ignores() ->
    [".git", ".hg", ".svn", ".idea"].


%%% Errors

-type error() :: {atomizer_cli_options, atomizer_cli_options:error()}
               | {atomizer_parse,       atomizer_parse:error()}
               | {atomizer_traverse,    atomizer_traverse:error()}.

-spec format_error(error()) -> io_lib:chars().
format_error({Module, Error}) -> Module:format_error(Error).


%%% Printing to standard outputs

print(Message) ->
    atomizer_output:put_chars(standard_io, [Message, "\n"]).

nl() ->
    atomizer_output:put_chars(standard_io, "\n").

-spec error(error()) -> ok.
error(Error) ->
    Message = [atomizer_output:bold("Error: ") | format_error(Error)],
    atomizer_output:put_chars(standard_error, [atomizer_output:red(Message), "\n"]).

-spec warning(error()) -> ok.
warning(Warning) ->
    case atomizer_cli_options:get_verbosity() of
        0 -> ok;
        _ ->
            Message = [atomizer_output:bold("Warning: ") | format_error(Warning)],
            atomizer_output:put_chars(standard_error, [atomizer_output:yellow(Message), "\n"])
    end.


%%% Miscellaneous

-spec pretty_atom(atom()) -> string().
pretty_atom(Atom) -> lists:flatten(io_lib:format("~p", [Atom])).

-spec words([io_lib:chars()]) -> io_lib:chars().
words(Words) ->
    lists:join(" ", Words).

-spec plural(non_neg_integer(), string(), string()) -> string().
plural(1, Singular, _) -> Singular;
plural(_, _, Plural)   -> Plural.
