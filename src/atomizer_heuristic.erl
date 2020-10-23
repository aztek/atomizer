-module(atomizer_heuristic).

-export([
    normalize/1,
    is_significant/1,
    is_loose/2
]).

-spec normalize(atom()) -> {ok, atomizer:normal_form()} | nok.
%% Normalize atom names spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination.
normalize(Atom) ->
    case string:lowercase([C || C <- atomizer:pretty_atom(Atom), C /= $_, C /= $-]) of
        [] -> nok;
        NormalForm -> {ok, list_to_atom(NormalForm)}
    end.

-spec is_significant(atom()) -> boolean().
is_significant(Atom) ->
    length(atom_to_list(Atom)) > 2.

-spec is_loose(atomizer:atom_info(), atomizer:atom_info()) -> {true, atomizer:loose_atom()} | false.
is_loose({_, LocationsA} = A, {_, LocationsB} = B) ->
    NrOccurrencesA = atomizer:nr_occurrences(LocationsA),
    NrOccurrencesB = atomizer:nr_occurrences(LocationsB),

    {{Loose, Lookalike}, Min, Max} =
        case NrOccurrencesA < NrOccurrencesB of
            true  -> {{A, B}, NrOccurrencesA, NrOccurrencesB};
            false -> {{B, A}, NrOccurrencesB, NrOccurrencesA}
        end,

    Disproportion = 4,
    Disproportional = Max > Min * Disproportion,

    case Disproportional andalso local(Loose) andalso related(A, B) of
        true  -> {true, {Loose, Lookalike}};
        false -> false
    end.

-spec local(atomizer:atom_info()) -> boolean().
local({_, Locations}) ->
    atomizer:nr_files(Locations) == 1.

-spec related(atomizer:atom_info(), atomizer:atom_info()) -> boolean().
related({_, LocationsA}, {_, LocationsB}) ->
    lists:any(fun (File) -> maps:is_key(File, LocationsB) end, maps:keys(LocationsA)).
