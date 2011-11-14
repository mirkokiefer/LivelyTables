-module(set_utils).
-export([set2records/1]).

-include("../include/records.hrl").

sets2records(Sets) -> [set2records(Set) || Set <- Sets].

set2records(Set) ->
  set2records(set_table(?SET, Set), Set).

set2records(?SET_OPERATION, Set) ->
  Sets = utils:row_coloumn(?COLOUMN_SETS, Set),
  set_operation2records(set_table(?SET_OPERATION, Set), sets2records(Sets), Set);

set2records(?FILTER, Set) ->
  Conditions = utils:row_coloumn(?COLOUMN_CONDITIONS, Set),
  FilterSet = utils:row_coloumn(?COLOUMN_SET, Set),
  #filter{set=set2records(FilterSet), conditions=conditions2records(Conditions)};

set2records(?TRANSFORM_SET, Set) ->
  TransformSet = utils:row_coloumn(?COLOUMN_SET, Set),
  set_transform2records(set_table(?TRANSFORM_SET, Set), set2records(TransformSet), Set);

set2records(?ROW_LIST, Set) -> utils:row_coloumn(?COLOUMN_ROWS, Set).

set_operation2records(?UNION, SetRecords, _Set) ->
  #union{sets=SetRecords};

set_operation2records(?INTERSECTION, SetRecords, _Set) ->
  #intersection{sets=SetRecords}.

set_transform2records(?TRANSFORM_CELLS_AS_TABLE, TransformSet, Set) ->
  ColoumnSet = utils:row_coloumn(?COLOUMN_COLOUMN_SET, Set),
  #rows2values{rows=TransformSet, coloumns=set2records(ColoumnSet)};

set_transform2records(?TRANSFORM_COLOUMNS_AS_TABLE, TransformSet, _Set) ->
  #rows2coloumns{rows=TransformSet};

set_transform2records(?TRANSFORM_ROWS_WITH_COLOUMNS, TransformSet, _Set) ->
  #coloumns2rows{coloumns=TransformSet};

set_transform2records(?TRANSFORM_ROWS_IN_TABLES, TransformSet, _Set) ->
  #tables2rows{tables=TransformSet}.

conditions2records(Conditions) ->
  [condition2record(Condition) || Condition <- Conditions].

condition2record(ConditionURI) ->
  Condition = store_interface:read_row(ConditionURI),
  Coloumns = utils:row_coloumn(?COLOUMN_COLOUMN_SET, Condition),
  condition2record(condition_table(Condition), set2records(Coloumns), Condition).

condition2record(?VALUE_CONDITION_EQUALS, Coloumns, Condition) ->
  Value = utils:row_coloumn(?COLOUMN_VALUE, Condition),
  #value_equals{coloumns=Coloumns, value=Value};

condition2record(?COLOUMN_EXISTS_CONDITION, Coloumns, _Condition) ->
  #coloumn_exists{coloumns=Coloumns}.

% utility functions
set_table(Set, _Set=#row{tables=Tables}) ->
  TableChain = Tables ++ lists:flatten([store_interface:read_subtables(Table) || Table <- Tables]),
  utils:filter_element(TableChain, store_interface:read_tables_including_directly(Set)).

condition_table(#row{tables=Tables}) ->
  utils:filter_element(Tables, store_interface:read_tables_including(?CONDITION)).