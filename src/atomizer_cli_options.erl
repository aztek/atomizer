-module(atomizer_cli_options).

-export([
    parse/1,
    init/1,
    package/1,
    get_action/0,
    get_verbosity/0,
    get_colors/0,
    get_warn_errors/0,
    format_error/1
]).

-export_type([
    options/0,
    action/0,
    verbosity/0,
    error/0
]).


%%% The type of command line options

-record(options, {
    action          = warn  :: action(),
    paths           = []    :: [file:filename()],
    ignores         = []    :: [file:filename()],
    includes        = []    :: [file:filename()],
    recursive       = true  :: boolean(),
    follow_symlinks = false :: boolean(),
    parse_beams     = false :: boolean(),
    warn_errors     = false :: boolean(),
    verbosity       = 1     :: verbosity(),
    colors          = false :: boolean()
}).
-opaque options() :: #options{}.

-type action() :: list | show | warn.
-type verbosity() :: 0 | 1 | 2. % 0 is least verbose, 2 is most verbose


%%% Parsing of command line options

-type error() :: {unrecognized_option, string()}
               | {missing_argument, action | verbosity}
               | {incorrect_value,  action | verbosity}.

-spec parse([string()]) -> {options, options()} | {message, string()} | {error, atomizer:error()}.
parse(CmdArgs) ->
    case parse(CmdArgs, #options{}) of
        {error, Error} -> {error, {?MODULE, Error}};
        Other -> Other
    end.

-spec parse([string()], options()) -> {options, options()} | {message, string()} | {error, error()}.
parse([], Options) -> {options, Options};
parse([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse(CmdArgs, Options#options{paths = [CmdArg | Options#options.paths]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_ignores([string()], options()) -> {ok, options()} | {message, string()} | {error, error()}.
parse_ignores([], Options) -> {options, Options};
parse_ignores([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse_ignores(CmdArgs, Options#options{ignores = [CmdArg | Options#options.ignores]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_includes([string()], options()) -> {ok, options()} | {message, string()} | {error, error()}.
parse_includes([], Options) -> {options, Options};
parse_includes([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse_includes(CmdArgs, Options#options{includes = [CmdArg | Options#options.includes]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_option(string(), [string()], options()) -> {ok, options()} | {message, string()} | {error, error()}.
parse_option(Option, CmdArgs, Options) ->
    case Option of
        _Help when Option == "h"; Option == "-help" ->
            {message, usage() ++ "\n" ++ help()};

        _Action when Option == "a"; Option == "-action" ->
            case parse_option_action(CmdArgs) of
                {ok, Action, TailCmdArgs} ->
                    parse(TailCmdArgs, Options#options{action = Action});

                {error, Error} ->
                    {error, Error}
            end;

        _Ignores when Option == "x"; Option == "-ignore" ->
            parse_ignores(CmdArgs, Options);

        _Includes when Option == "i"; Option == "I"; Option == "-include" ->
            parse_includes(CmdArgs, Options);

        _IncludeFile when Option == "-include-file" ->
            case parse_option_include_file(CmdArgs) of
                {ok, Includes, TailCmdArgs} ->
                    parse(TailCmdArgs, Options#options{includes = Includes});

                {error, Error} ->
                    {error, Error}
            end;

        _Recursive when Option == "r"; Option == "-non-recursive" ->
            parse(CmdArgs, Options#options{recursive = false});

        _FollowSymlinks when Option == "l"; Option == "-follow-symlinks" ->
            parse(CmdArgs, Options#options{follow_symlinks = true});

        _ParseBeams when Option == "b"; Option == "-parse-beams" ->
            parse(CmdArgs, Options#options{parse_beams = true});

        _Warnings when Option == "w"; Options == "-warn-errors" ->
            parse(CmdArgs, Options#options{warn_errors = true});

        _Verbosity when Option == "v"; Option == "-verbosity" ->
            case parse_option_verbosity(CmdArgs) of
                {ok, Verbosity, TailCmdArgs} ->
                    parse(TailCmdArgs, Options#options{verbosity = Verbosity});

                {error, Error} ->
                    {error, Error}
            end;

        _Colors when Option == "c"; Option == "-colors" ->
            parse(CmdArgs, Options#options{colors = true});

        _ ->
            {error, {unrecognized_option, Option}}
    end.

-spec parse_option_action([string()]) -> {ok, action(), [string()]} | {error, error()}.
parse_option_action([]) -> {error, {missing_argument, action}};
parse_option_action([CmdArg | CmdArgs]) ->
    case parse_action(CmdArg) of
        {ok, Action} -> {ok, Action, CmdArgs};
        nok -> {error, {incorrect_value, action}}
    end.

-spec parse_action(string()) -> {ok, action()} | nok.
parse_action("list") -> {ok, list};
parse_action("show") -> {ok, show};
parse_action("warn") -> {ok, warn};
parse_action(_)      -> nok.

-spec parse_option_include_file([string()]) -> {ok, [file:filename()], [string()]} | {error, error()}.
parse_option_include_file([]) -> {error, {missing_argument, include_file}};
parse_option_include_file([CmdArg | CmdArgs]) ->
    case parse_include_file(CmdArg) of
        {ok, Includes} -> {ok, Includes, CmdArgs};
        nok -> {error, {file_error, CmdArg}}
    end.

-spec parse_include_file(file:filename()) -> {ok, [file:filename()]} | nok.
parse_include_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> {ok, string:tokens(erlang:binary_to_list(Binary), "\n")};
        {error, _Reason} -> nok
    end.

-spec parse_option_verbosity([string()]) -> {ok, verbosity(), [string()]} | {error, error()}.
parse_option_verbosity([]) -> {error, {missing_argument, verbosity}};
parse_option_verbosity([CmdArg | CmdArgs]) ->
    case parse_verbosity(CmdArg) of
        {ok, Verbosity} -> {ok, Verbosity, CmdArgs};
        nok -> {error, {incorrect_value, verbosity}}
    end.

-spec parse_verbosity(string()) -> {ok, verbosity()} | nok.
parse_verbosity("0") -> {ok, 0};
parse_verbosity("1") -> {ok, 1};
parse_verbosity("2") -> {ok, 2};
parse_verbosity(_)   -> nok.

usage() ->
    "Usage: atomizer [-a | --action ACTION] [-b | --parse-beams]\n"
    "                [-x | --ignore PATH*] [-i | -I | --include PATH* | --include-file PATH]\n"
    "                [-r | --non-recursive] [-l | --follow-symlinks] [-w | --warn-errors]\n"
    "                [-v | --verbosity VERBOSITY] [-c | --colors] PATH*\n".

help() ->
    "".


%%%

-spec package(options()) -> atomizer:package().
package(Options) ->
    #options{
        paths           = Paths,
        ignores         = Ignores,
        includes        = Includes,
        recursive       = Recursive,
        follow_symlinks = FollowSymlinks,
        parse_beams     = ParseBeams
    } = Options,
    atomizer:package(Paths, Ignores, Includes, Recursive, FollowSymlinks, ParseBeams).


%%% Global dictionary of command line arguments populated at startup

-define(CLI_TABLE, cli).

-spec init(options()) -> ok.
init(Options) ->
    ets:new(?CLI_TABLE, [set, protected, named_table]),
    ets:insert(?CLI_TABLE, {options, Options}),
    ok.

-spec get_options() -> options().
get_options() ->
    case ets:lookup(?CLI_TABLE, options) of
        [{options, Options}] -> Options;
        _ -> #options{}
    end.

-spec get_warn_errors() -> boolean().
get_warn_errors() ->
    Options = get_options(),
    Options#options.warn_errors.

-spec get_verbosity() -> verbosity().
get_verbosity() ->
    Options = get_options(),
    Options#options.verbosity.

-spec get_colors() -> boolean().
get_colors() ->
    Options = get_options(),
    Options#options.colors.

-spec get_action() -> action().
get_action() ->
    Options = get_options(),
    Options#options.action.

-spec format_error(error()) -> io_lib:chars().
format_error({unrecognized_option, Option}) ->
    ["Unrecognized option ", atomizer_color:bold(["-", Option]), "."];

format_error({missing_argument, Option}) ->
    ["Malformed arguments - ", atom_to_list(Option), "is missing."];

format_error({incorrect_value, Option}) ->
    SupportedValues = case Option of
                          action    -> "list, show and warn";
                          verbosity -> "0, 1 and 2"
                      end,
    ["Wrong ", atom_to_list(Option), ". Supported values are ", SupportedValues, "."].
