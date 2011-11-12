-module(set_interface).
-export([eval_set/1, set2records/1]).

-include("../include/records.hrl").

eval_set(Set) ->
  eval_set_type(set_type(?SET, Set), Set).

eval_set_type(?SET_OPERATION, Set) ->
  Sets = [sets:from_list(eval_set(Each)) || Each <- utils:item_property(?PROPERTY_SETS, Set)],
  eval_set_operation(set_type(?SET_OPERATION, Set), Sets, Set);

eval_set_type(?FILTER, Set) -> 
  Conditions = utils:item_property(?PROPERTY_CONDITIONS, Set),
  Items = eval_set(utils:item_property(?PROPERTY_SET, Set)),
  [Item || Item <- Items, passes_conditions(Item, Conditions)];

eval_set_type(?TRANSFORM_SET, Set) ->
  Items = eval_set(utils:item_property(?PROPERTY_SET, Set)),
  eval_set_transform(set_type(?TRANSFORM_SET, Set), Items, Set);

eval_set_type(?ITEM_LIST, Set) -> utils:item_property(?PROPERTY_ITEMS, Set).

eval_set_operation(?UNION, Sets, _Set) -> sets:to_list(sets:union(Sets));

eval_set_operation(?INTERSECTION, Sets, _Set) -> sets:to_list(sets:intersection(Sets)).

eval_set_transform(?TRANSFORM_ITEMS_TO_VALUES, ItemURIs, TransformSet) ->
  UsedProperties = eval_set(utils:item_property(?PROPERTY_PROPERTY_SET, TransformSet)),
  lists:flatten([item_values(ItemURI, UsedProperties) || ItemURI <- ItemURIs]);

eval_set_transform(?TRANSFORM_ITEMS_TO_PROPERTIES, ItemURIs, _TransformSet) ->
  lists:flatten([item_properties(ItemURI) || ItemURI <- ItemURIs]);

eval_set_transform(?TRANSFORM_PROPERTIES_TO_ITEMS, PropertyURIs, _TransformSet) ->
  ValidTypes = utils:types_with_legal_properties(PropertyURIs),
  utils:set(lists:flatten([store_interface:read_items_of_type(Each) || Each <- ValidTypes]));

eval_set_transform(?TRANSFORM_TYPES_TO_ITEMS, Types, _TransformSet) ->
  utils:set(lists:flatten([store_interface:read_items_of_type(Type) || Type <- Types])).

passes_conditions(ItemURI, Conditions) -> 
  lists:all(fun(Condition) -> passes_condition(ItemURI, Condition) end, Conditions).

passes_condition(ItemURI, ConditionURI) ->
  Condition = store_interface:read_item(ConditionURI),
  Properties = eval_set(utils:item_property(?PROPERTY_PROPERTY_SET, Condition)),
  Item = store_interface:read_item(ItemURI),
  passes_condition(condition_type(Condition), Item, Properties, Condition).

passes_condition(?PROPERTY_EXISTS_CONDITION, #item{properties=Properties}, CheckProperties, _Condition) ->
  PropertyList = [Property || {Property, _Value} <- Properties],
  utils:is_joint(PropertyList, CheckProperties);

passes_condition(?VALUE_CONDITION_EQUALS, Item, CheckProperties, Condition) ->
  Properties = [{Property, Value} || {Property, Value} <- Item#item.properties,
    lists:member(Property, CheckProperties)],
  ValidValue = utils:item_property(?PROPERTY_VALUE, Condition),
  lists:all(fun({_Property, Value}) -> Value == ValidValue end, Properties).

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
