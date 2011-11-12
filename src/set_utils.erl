-module(set_utils).
-export([set2records/1]).

-include("../include/records.hrl").

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