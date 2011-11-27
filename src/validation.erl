%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(validation, [Store]).

-export([check/1]).

-include("../include/records.hrl").

check(Table = #table{}) -> validate_row(utils:table2row(Table));
check(Coloumn = #coloumn{}) -> validate_row(utils:coloumn2row(Coloumn));
check(Row = #row{}) -> validate_row(Row).

validate_row(Row) ->
  {ValidTable, TableErrors} = validate_table_requirements(Row),
  {ValidColoumns, ColoumnErrors} = validate_coloumns(Row),
  {ValidTable and ValidColoumns, TableErrors++ColoumnErrors}.

validate_table_requirements(Row=#row{tables=Tables}) ->
  LegalCols = lists:flatten([Store:read_coloumns_of_table(Table) || Table <- Tables]),
  Results = [validate_legal_coloumns(LegalColoumns, Row) || #table{legal_coloumns=LegalColoumns} <- LegalCols],
  sum_result(Results).

validate_legal_coloumns(Coloumns, Row) -> validate_legal_coloumns(Coloumns, Row, {true, []}).
validate_legal_coloumns([First|Rest], Row, {SumValid, SumErrors}) ->
  {Valid, Errors} = validate_legal_coloumn(First, Row),
  validate_legal_coloumns(Rest, Row, {SumValid and Valid, Errors++SumErrors});
validate_legal_coloumns([], _, Result) -> Result.

validate_legal_coloumn(?COLOUMN_LABEL, #row{}) -> {true, []};
validate_legal_coloumn(?COLOUMN_TABLES, #row{}) -> {true, []};
validate_legal_coloumn(LegalColoumn, #row{coloumns=Coloumns}) ->
  ColoumnURIs = [ColoumnURI || {ColoumnURI, _} <- Coloumns],
  case lists:member(LegalColoumn, ColoumnURIs) of
    true -> {true, []};
    false -> case Store:read_coloumn(LegalColoumn) of
      #coloumn{optional=false} -> {false, legal_coloumn_missing(LegalColoumn)};
      #coloumn{optional=true} -> {true, []}
    end
  end.

validate_coloumns(Row=#row{tables=Tables, coloumns=Coloumns}) ->
  {TablesExist, TableErrors} = case Tables of
    [] -> {false, legal_coloumn_missing(?COLOUMN_TABLES)};
    _ -> {true, []}
  end,
  {ValuesValid, ValuesErrors} = validate_coloumn_values(Coloumns, Row),
  {TablesExist and ValuesValid, TableErrors ++ ValuesErrors}.

validate_coloumn_values(Coloumns, Row) -> validate_coloumn_values(Coloumns, Row, {true, []}).

validate_coloumn_values([{ColoumnURI, Value}|Rest], Row, {Valid, Errors}) ->
  {ValidColoumn, ColoumnErrors} = case Store:read_coloumn(ColoumnURI) of
    undefined -> {false, coloumn_not_exists(ColoumnURI, Value)};
    #coloumn{range=Range, arity=Arity, optional=Optional} -> 
      Result = validate_coloumn_range(Range, Arity, Optional, Value, Row),
      case Result of
        {true, _} -> {true, []};
        {false, RangeErrors} -> {false, invalid_coloumn_value(Row, ColoumnURI, Range, RangeErrors)}
      end
  end,
  validate_coloumn_values(Rest, Row, {Valid and ValidColoumn, ColoumnErrors ++ Errors});
validate_coloumn_values([], _Row, Result) -> Result.

validate_coloumn_range(_Range, _Arity, true, undefined, _Row) -> {true, []};
validate_coloumn_range(_Range, ?ARITY_MANY, _, [], _Row) -> {true, []};
validate_coloumn_range(Range, ?ARITY_MANY, Optional, Values=[_|_], Row) ->
  Results = [validate_coloumn_range(Range, ?ARITY_ONE, Optional, Value, Row) || Value <- Values],
  sum_result(Results);
validate_coloumn_range(_Range, ?ARITY_MANY, _, _Value, _Row) -> {false, arity_error()};
validate_coloumn_range(?COLOUMN_TYPE_STRING, ?ARITY_ONE, _, Value, _Row) -> {is_bitstring(Value), []};
validate_coloumn_range(?COLOUMN_TYPE_NUMBER, ?ARITY_ONE, _, Value, _Row) -> {is_number(Value), []};
validate_coloumn_range(?COLOUMN_TYPE_BOOLEAN, ?ARITY_ONE, _, Value, _Row) -> {is_boolean(Value), []};
validate_coloumn_range(_, ?ARITY_ONE, _, ?COLOUMN_TYPE_NUMBER, _Row) -> {true, []};
validate_coloumn_range(_, ?ARITY_ONE, _, ?COLOUMN_TYPE_STRING, _Row) -> {true, []};
validate_coloumn_range(_, ?ARITY_ONE, _, ?COLOUMN_TYPE_BOOLEAN, _Row) -> {true, []};

validate_coloumn_range(Range, ?ARITY_ONE, Optional, Value=#table{}, Row) ->
  validate_coloumn_range(Range, ?ARITY_ONE, Optional, utils:table2row(Value), Row);

validate_coloumn_range(Range, ?ARITY_ONE, Optional, Value=#coloumn{}, Row) ->
  validate_coloumn_range(Range, ?ARITY_ONE, Optional, utils:coloumn2row(Value), Row);

validate_coloumn_range(Range, ?ARITY_ONE, _, Value=#row{tables=Tables}, _Row) ->
  case validate_row(Value) of
    {true, _} ->
      Parents = Tables ++ lists:flatten([Store:read_parent_tables(Table) || Table <- Tables]),
      {lists:member(Range, Parents), []};
    {false, Errors} -> {false, Errors}
  end;
validate_coloumn_range(Range, ?ARITY_ONE, _, Value, _Row) ->
  {lists:member(Range, Store:read_tables_of_row(Value)), []}.

%Merges Error results
sum_result(Result) -> sum_result(Result, {true, []}).
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.

%Error messages
legal_coloumn_missing(LegalColoumn) -> [[{message, <<"Coloumn missing on row">>}, {value, LegalColoumn}]].
coloumn_not_exists(Coloumn, Value) ->
  [[{message, <<"Coloumn does not exist">>}, {coloumn, Coloumn}, {value, Value}]].
invalid_coloumn_value(Row=#row{uri=URI}, Coloumn, Range, []) ->
  [[{message, <<"Invalid coloumn value">>}, {row, Row}, {range, Range}, {coloumn,Coloumn}]];
invalid_coloumn_value(Row=#row{uri=URI}, Coloumn, Range, Details) ->
  [[{message, <<"Invalid coloumn value">>}, {row, Row}, {range, Range}, {coloumn,Coloumn},
    {details, Details}]].
arity_error() -> [[{message, <<"Arity is 'many' but only single value">>}]].