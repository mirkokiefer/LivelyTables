%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(validation).

-export([check/1, check/2]).

-include("../include/records.hrl").

check(Table = #table{}) -> check(utils:table2row(Table));
check(Column = #column{}) -> check(utils:column2row(Column));
check(Row = #row{uri=URI}) -> check(Row, utils:row_uri2table_uri(URI)).

check(Row, TableURI) ->
  {ValidTable, TableErrors} = validate_table_requirements(Row, TableURI),
  {ValidColumns, ColumnErrors} = validate_columns(Row),
  {ValidTable and ValidColumns, TableErrors++ColumnErrors}.

validate_table_requirements(Row=#row{}, TableURI) ->
  LegalColumns = global_interface:table_columns(TableURI),
  validate_legal_columns(LegalColumns, Row).

validate_legal_columns(Columns, Row) -> validate_legal_columns(Columns, Row, {true, []}).
validate_legal_columns([First|Rest], Row, {SumValid, SumErrors}) ->
  {Valid, Errors} = validate_legal_column(First, Row),
  validate_legal_columns(Rest, Row, {SumValid and Valid, Errors++SumErrors});
validate_legal_columns([], _, Result) -> Result.

validate_legal_column(?COLUMN_LABEL, #row{}) -> {true, []};
validate_legal_column(LegalColumn, #row{columns=Columns}) ->
  ColumnURIs = [ColumnURI || {ColumnURI, _} <- Columns],
  case lists:member(LegalColumn, ColumnURIs) of
    true -> {true, []};
    false -> case global_interface:read_cell(?COLUMN_OPTIONAL, LegalColumn) of
      false -> {false, legal_column_missing(LegalColumn)};
      true -> {true, []}
    end
  end.

validate_columns(Row=#row{columns=Columns}) ->
  validate_column_values(Columns, Row, {true, []}).

validate_column_values([{ColumnURI, Value}|Rest], Row, {Valid, Errors}) ->
  {ValidColumn, ColumnErrors} = case utils:row2column(global_interface:read_row(ColumnURI)) of
    undefined -> {false, column_not_exists(ColumnURI, Value)};
    #column{range=Range, arity=Arity, optional=Optional} -> 
      Result = validate_column_range(Range, Arity, Optional, Value, Row),
      case Result of
        {true, _} -> {true, []};
        {false, RangeErrors} -> {false, invalid_column_value(Row, ColumnURI, Range, RangeErrors)}
      end
  end,
  validate_column_values(Rest, Row, {Valid and ValidColumn, ColumnErrors ++ Errors});
validate_column_values([], _Row, Result) -> Result.

validate_column_range(_Range, _Arity, true, undefined, _Row) -> {true, []};
validate_column_range(_Range, ?ARITY_MANY, _, [], _Row) -> {true, []};
validate_column_range(Range, ?ARITY_MANY, Optional, Values=[_|_], Row) ->
  Results = [validate_column_range(Range, ?ARITY_ONE, Optional, Value, Row) || Value <- Values],
  sum_result(Results);
validate_column_range(_Range, ?ARITY_MANY, _, _Value, _Row) -> {false, arity_error()};
validate_column_range(?COLUMN_TYPE_STRING, ?ARITY_ONE, _, Value, _Row) -> {is_bitstring(Value), []};
validate_column_range(?COLUMN_TYPE_NUMBER, ?ARITY_ONE, _, Value, _Row) -> {is_number(Value), []};
validate_column_range(?COLUMN_TYPE_BOOLEAN, ?ARITY_ONE, _, Value, _Row) -> {is_boolean(Value), []};
validate_column_range(_, ?ARITY_ONE, _, ?COLUMN_TYPE_NUMBER, _Row) -> {true, []};
validate_column_range(_, ?ARITY_ONE, _, ?COLUMN_TYPE_STRING, _Row) -> {true, []};
validate_column_range(_, ?ARITY_ONE, _, ?COLUMN_TYPE_BOOLEAN, _Row) -> {true, []};

validate_column_range(Range, ?ARITY_ONE, Optional, Value=#table{}, Row) ->
  validate_column_range(Range, ?ARITY_ONE, Optional, utils:table2row(Value), Row);

validate_column_range(Range, ?ARITY_ONE, Optional, Value=#column{}, Row) ->
  validate_column_range(Range, ?ARITY_ONE, Optional, utils:column2row(Value), Row);

validate_column_range(Range, ?ARITY_ONE, _, Value=#row{}, _Row) -> check(Value, Range);

validate_column_range(Range, ?ARITY_ONE, _, RowURI=#row_uri{}, _Row) ->
  ValuesTable = utils:row_uri2table_uri(RowURI),
  {lists:member(Range, global_interface:flat_table_hierarchy(ValuesTable)), []}.

%Merges Error results
sum_result(Result) -> sum_result(Result, {true, []}).
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.

%Error messages
legal_column_missing(LegalColumn) -> [[{message, <<"Column missing on row">>}, {value, LegalColumn}]].
column_not_exists(Column, Value) ->
  [[{message, <<"Column does not exist">>}, {column, Column}, {value, Value}]].
invalid_column_value(Row=#row{uri=_URI}, Column, Range, []) ->
  [[{message, <<"Invalid column value">>}, {row, Row}, {range, Range}, {column,Column}]];
invalid_column_value(Row=#row{uri=_URI}, Column, Range, Details) ->
  [[{message, <<"Invalid column value">>}, {row, Row}, {range, Range}, {column,Column},
    {details, Details}]].
arity_error() -> [[{message, <<"Arity is 'many' but only single value">>}]].