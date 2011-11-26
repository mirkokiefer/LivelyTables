%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc Set Interface
%%% @end
%%%-------------------------------------------------------------------

-module(set_interface, [StoreInterface]).
-export([eval/1, eval_set_row/1]).

-include("../include/records.hrl").

eval_set_row(Set) ->
  SetUtils = set_utils:new(StoreInterface),
  eval(SetUtils:set2records(Set)).

eval(#union{sets=Sets}) ->
  RowSets = sets2erlang_sets(Sets),
  sets:to_list(sets:union(RowSets));

eval(#intersection{sets=Sets}) ->
  RowSets = sets2erlang_sets(Sets),
  sets:to_list(sets:intersection(RowSets));

eval(#filter{set=Set, conditions=Conditions}) ->
  [RowURI || RowURI <- eval(Set), passes_conditions(read_row(RowURI), Conditions)];

eval(#rows2values{rows=Set, coloumns=Coloumns}) ->
  lists:flatten([row_values(RowURI, Coloumns) || RowURI <- eval(Set)]);

eval(#rows2coloumns{rows=Set}) ->
  lists:flatten([row_coloumns(RowURI) || RowURI <- eval(Set)]);

eval(#coloumns2rows{coloumns=Set}) ->
  ValidTables = utils:tables_with_legal_coloumns(eval(Set), StoreInterface),
  utils:set(lists:flatten([StoreInterface:read_rows_of_table(Each) || Each <- ValidTables]));

eval(#tables2rows{tables=Set}) ->
  utils:set(lists:flatten([StoreInterface:read_rows_of_table(Table) || Table <- eval(Set)]));

eval(RowList) -> RowList.

passes_conditions(Row, Conditions) -> 
  lists:all(fun(Condition) -> passes_condition(Condition, Row) end, Conditions).

passes_condition(#coloumn_exists{coloumns=RequiredColoumns}, #row{coloumns=Coloumns}) ->
  ColoumnList = [Coloumn || {Coloumn, _Value} <- Coloumns],
  utils:is_joint(ColoumnList, RequiredColoumns);

passes_condition(#value_equals{coloumns=CheckColoumns, value=ValidValue}, #row{coloumns=Coloumns}) ->
  ColoumnsToCheck = [{Coloumn, Value} || {Coloumn, Value} <- Coloumns,
    lists:member(Coloumn, CheckColoumns)],
  lists:all(fun({_Coloumn, Value}) -> Value == ValidValue end, ColoumnsToCheck).


% utility functions
row_values(RowURI, UsedColoumns) ->
  Row = StoreInterface:read_row(RowURI),
  [Value || {Coloumn, Value} <- Row#row.coloumns, lists:member(Coloumn, UsedColoumns)].

row_coloumns(RowURI) ->
  #row{coloumns=Coloumns} = StoreInterface:read_row(RowURI),
  [Coloumn || {Coloumn, _} <- Coloumns].

read_row(URI) -> StoreInterface:read_row(URI).

sets2erlang_sets(Sets) -> [sets:from_list(eval(Each)) || Each <- Sets].