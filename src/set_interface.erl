-module(set_interface).
-export([eval/1, eval_set_item/1]).

-include("../include/records.hrl").

eval_set_item(Set) ->
  eval(set_utils:set2records(Set)).

eval(#union{sets=Sets}) ->
  ItemSets = sets2erlang_sets(Sets),
  sets:to_list(sets:union(ItemSets));

eval(#intersection{sets=Sets}) ->
  ItemSets = sets2erlang_sets(Sets),
  sets:to_list(sets:intersection(ItemSets));

eval(#filter{set=Set, conditions=Conditions}) ->
  [ItemURI || ItemURI <- eval(Set), passes_conditions(read_item(ItemURI), Conditions)];

eval(#items2values{items=Set, properties=Properties}) ->
  lists:flatten([item_values(ItemURI, Properties) || ItemURI <- eval(Set)]);

eval(#items2properties{items=Set}) ->
  lists:flatten([item_properties(ItemURI) || ItemURI <- eval(Set)]);

eval(#properties2items{properties=Set}) ->
  ValidTypes = utils:types_with_legal_properties(eval(Set)),
  utils:set(lists:flatten([store_interface:read_items_of_type(Each) || Each <- ValidTypes]));

eval(#types2items{types=Set}) ->
  utils:set(lists:flatten([store_interface:read_items_of_type(Type) || Type <- eval(Set)]));

eval(ItemList) -> ItemList.

passes_conditions(Item, Conditions) -> 
  lists:all(fun(Condition) -> passes_condition(Condition, Item) end, Conditions).

passes_condition(#property_exists{properties=RequiredProperties}, #item{properties=Properties}) ->
  PropertyList = [Property || {Property, _Value} <- Properties],
  utils:is_joint(PropertyList, RequiredProperties);

passes_condition(#value_equals{properties=CheckProperties, value=ValidValue}, #item{properties=Properties}) ->
  PropertiesToCheck = [{Property, Value} || {Property, Value} <- Properties,
    lists:member(Property, CheckProperties)],
  lists:all(fun({_Property, Value}) -> Value == ValidValue end, PropertiesToCheck).


% utility functions
item_values(ItemURI, UsedProperties) ->
  Item = store_interface:read_item(ItemURI),
  [Value || {Property, Value} <- Item#item.properties, lists:member(Property, UsedProperties)].

item_properties(ItemURI) ->
  #item{properties=Properties} = store_interface:read_item(ItemURI),
  [Property || {Property, _} <- Properties].

read_item(URI) -> store_interface:read_item(URI).

sets2erlang_sets(Sets) -> [sets:from_list(eval(Each)) || Each <- Sets].