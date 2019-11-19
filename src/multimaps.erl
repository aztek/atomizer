-module(multimaps).

-export_type([multimap/2]).

-opaque multimap(Key, Value) :: #{Key => sets:set(Value)}.

-export([empty/0, from_list/1, put/3]).

-spec empty() -> multimap(Key :: term(), Value :: term()).
empty() -> #{}.

-spec from_list([{Key, Value}]) -> multimap(Key, Value) when
    Key   :: term(),
    Value :: term().
from_list(List) ->
    lists:foldl(fun ({Key, Value}, Multimap) -> put(Key, Value, Multimap) end, empty(), List).

-spec put(Key, Value, multimap(Key, Value)) -> multimap(Key, Value) when
    Key   :: term(),
    Value :: term().
put(Key, Value, Multimap) ->
    Set = maps:get(Key, Multimap, sets:new()),
    maps:put(Key, sets:add_element(Value, Set), Multimap).
