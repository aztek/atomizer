-module(atomizer_cli_options).

-export([
    parse/1,
    init/1,
    package/1,
    get_action/0,
    get_verbosity/0,
    get_warn_errors/0
]).

-export_type([
    options/0,
    action/0,
    verbosity/0
]).


%%% The type of command line options

-record(options, {
    action      = warn  :: action(),
    paths       = []    :: [file:filename()],
    includes    = []    :: [file:filename()],
    parse_beams = false :: boolean(),
    warn_errors = false :: boolean(),
    verbosity   = 1     :: verbosity()
}).
-opaque options() :: #options{}.

-type action() :: list | show | warn.
-type verbosity() :: 0 | 1 | 2. % 0 is least verbose, 2 is most verbose


%%% Parsing of command line options

-spec parse([string()]) -> {options, options()} | {message, string()} | {error, string()}.
parse(CmdArgs) -> parse(CmdArgs, #options{}).

-spec parse([string()], options()) -> {options, options()} | {message, string()} | {error, string()}.
parse([], Options) -> {options, Options};
parse([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse(CmdArgs, Options#options{paths = [CmdArg | Options#options.paths]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_includes([string()], options()) -> {ok, options()} | {message, string()} | {error, string()}.
parse_includes([], Options) -> {options, Options};
parse_includes([CmdArg | CmdArgs], Options) ->
    case string:prefix(CmdArg, "-") of
        nomatch -> parse_includes(CmdArgs, Options#options{includes = [CmdArg | Options#options.includes]});
        Option  -> parse_option(Option, CmdArgs, Options)
    end.

-spec parse_option(string(), [string()], options()) -> {ok, options()} | {message, string()} | {error, string()}.
parse_option(Option, CmdArgs, Options) ->
    case Option of
        _Help when Option == "h"; Option == "-help" ->
            {message, usage() ++ "\n" ++ help()};

        _Action when Option == "a"; Option == "-action" ->
            case parse_action(CmdArgs) of
                {ok, Action, TailCmdArgs} ->
                    parse(TailCmdArgs, Options#options{action = Action});

                {error, Error} ->
                    {error, Error}
            end;

        _Includes when Option == "i"; Option == "I"; Option == "-include" ->
            parse_includes(CmdArgs, Options);

        _ParseBeams when Option == "b"; Option == "-parse-beams" ->
            parse(CmdArgs, Options#options{parse_beams = true});

        _Warnings when Option == "w"; Options == "-warn-errors" ->
            parse(CmdArgs, Options#options{warn_errors = true});

        _Verbosity when Option == "v"; Option == "-verbosity" ->
            case parse_verbosity(CmdArgs) of
                {ok, Verbosity, TailCmdArgs} ->
                    parse(TailCmdArgs, Options#options{verbosity = Verbosity});

                {error, Error} ->
                    {error, Error}
            end;

        _ ->
            {error, "Unrecognized option."}
    end.

-spec parse_action([string()]) -> {ok, action(), [string()]} | {error, string()}.
parse_action([]) -> {error, "Malformed arguments - action is missing."};
parse_action([CmdArg | CmdArgs]) ->
    case CmdArg of
        "list" -> {ok, list, CmdArgs};
        "show" -> {ok, show, CmdArgs};
        "warn" -> {ok, warn, CmdArgs};
        _ -> {error, "Wrong action. Supported values are list, show and warn."}
    end.

-spec parse_verbosity([string()]) -> {ok, verbosity(), [string()]} | {error, string()}.
parse_verbosity([]) -> {error, "Malformed arguments - verbosity is missing."};
parse_verbosity([CmdArg | CmdArgs]) ->
    case CmdArg of
        "0" -> {ok, 0, CmdArgs};
        "1" -> {ok, 1, CmdArgs};
        "2" -> {ok, 2, CmdArgs};
        _ -> {error, "Wrong verbosity. Supported values are 0, 1 and 2."}
    end.

usage() ->
    "Usage: atomizer [-a | --action ACTION] [-b | --parse-beams] [-i | -I | --include PATH*]\n" ++
    "                [-w | --warn-errors] [-v | --verbosity VERBOSITY] PATH*\n".

help() ->
    "".


%%%

-spec package(options()) -> atomizer:package().
package(#options{paths = Paths, includes = Includes, parse_beams = ParseBeams}) ->
    atomizer:package(Paths, Includes, ParseBeams).


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

-spec get_action() -> action().
get_action() ->
    Options = get_options(),
    Options#options.action.
