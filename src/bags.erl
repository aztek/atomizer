-module(bags).

-export_type([bag/2]).

-opaque bag(Key, Value) :: #{Key => sets:set(Value)}.

-export([empty/0, from_list/1, put/3, append_bags/2]).

-spec empty() -> bag(Key :: term(), Value :: term()).
empty() -> #{}.

-spec from_list([{Key, Value}]) -> bag(Key, Value) when
  Key   :: term(),
  Value :: term().
from_list(List) ->
  lists:foldl(fun ({Key, Value}, Bag) -> put(Key, Value, Bag) end,
              empty(), List).

-spec put(Key, Value, bag(Key, Value)) -> bag(Key, Value) when
  Key :: term(),
  Value :: term().
put(Key, Value, Bag) ->
  Set = maps:get(Key, Bag, sets:new()),
  maps:put(Key, sets:add_element(Value, Set), Bag).

-spec append_bags(bag(Key, Value), bag(Key, Value)) -> bag(Key, Value) when
  Key   :: term(),
  Value :: term().
append_bags(Bag1, Bag2) ->
  maps:fold(fun (Key, Set, Bag) ->
    maps:update_with(Key, fun (Set1) -> sets:union(Set, Set1) end, Set, Bag)
  end, Bag2, Bag1).
