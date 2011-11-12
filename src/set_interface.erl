-module(set_interface).
-export([eval/1, set2records/1]).

-include("../include/records.hrl").

eval(#union{sets=Sets}) ->
  ItemSets = [sets:from_list(eval(Each)) || Each <- Sets],
  sets:to_list(sets:union(ItemSets));

eval(#intersection{sets=Sets}) ->
  ItemSets = [sets:from_list(eval(Each)) || Each <- Sets],
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

item_values(ItemURI, UsedProperties) ->
  Item = store_interface:read_item(ItemURI),
  [Value || {Property, Value} <- Item#item.properties, lists:member(Property, UsedProperties)].

item_properties(ItemURI) ->
  #item{properties=Properties} = store_interface:read_item(ItemURI),
  [Property || {Property, _} <- Properties].


% Set to records
sets2records(Sets) -> [set2records(Set) || Set <- Sets].

set2records(Set) ->
  set2records(set_type(?SET, Set), Set).

set2records(?SET_OPERATION, Set) ->
  Sets = utils:item_property(?PROPERTY_SETS, Set),
  set_operation2records(set_type(?SET_OPERATION, Set), sets2records(Sets), Set);

set2records(?FILTER, Set) ->
  Conditions = utils:item_property(?PROPERTY_CONDITIONS, Set),
  FilterSet = utils:item_property(?PROPERTY_SET, Set),
  #filter{set=set2records(FilterSet), conditions=conditions2records(Conditions)};

set2records(?TRANSFORM_SET, Set) ->
  TransformSet = utils:item_property(?PROPERTY_SET, Set),
  set_transform2records(set_type(?TRANSFORM_SET, Set), set2records(TransformSet), Set);

set2records(?ITEM_LIST, Set) -> utils:item_property(?PROPERTY_ITEMS, Set).

set_operation2records(?UNION, SetRecords, _Set) ->
  #union{sets=SetRecords};

set_operation2records(?INTERSECTION, SetRecords, _Set) ->
  #intersection{sets=SetRecords}.

set_transform2records(?TRANSFORM_ITEMS_TO_VALUES, TransformSet, Set) ->
  PropertySet = utils:item_property(?PROPERTY_PROPERTY_SET, Set),
  #items2values{items=TransformSet, properties=set2records(PropertySet)};

set_transform2records(?TRANSFORM_ITEMS_TO_PROPERTIES, TransformSet, _Set) ->
  #items2properties{items=TransformSet};

set_transform2records(?TRANSFORM_PROPERTIES_TO_ITEMS, TransformSet, _Set) ->
  #properties2items{properties=TransformSet};

set_transform2records(?TRANSFORM_TYPES_TO_ITEMS, TransformSet, _Set) ->
  #types2items{types=TransformSet}.

conditions2records(Conditions) ->
  [condition2record(Condition) || Condition <- Conditions].

condition2record(ConditionURI) ->
  Condition = store_interface:read_item(ConditionURI),
  Properties = utils:item_property(?PROPERTY_PROPERTY_SET, Condition),
  condition2record(condition_type(Condition), set2records(Properties), Condition).

condition2record(?VALUE_CONDITION_EQUALS, Properties, Condition) ->
  Value = utils:item_property(?PROPERTY_VALUE, Condition),
  #value_equals{properties=Properties, value=Value};

condition2record(?PROPERTY_EXISTS_CONDITION, Properties, _Condition) ->
  #property_exists{properties=Properties}.

% utility functions
set_type(Set, _Set=#item{types=Types}) ->
  TypeChain = Types ++ lists:flatten([store_interface:read_parents(Type) || Type <- Types]),
  utils:filter_element(TypeChain, store_interface:read_direct_subtypes(Set)).

condition_type(#item{types=Types}) ->
  utils:filter_element(Types, store_interface:read_subtypes(?CONDITION)).

read_item(URI) -> store_interface:read_item(URI).