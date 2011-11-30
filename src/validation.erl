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
check(Coloumn = #coloumn{}) -> check(utils:coloumn2row(Coloumn));
check(Row = #row{uri=URI}) -> check(Row, utils:row_uri2table_uri(URI)).

check(Row, TableURI) ->
  {ValidTable, TableErrors} = validate_table_requirements(Row, TableURI),
  {ValidColoumns, ColoumnErrors} = validate_coloumns(Row),
  {ValidTable and ValidColoumns, TableErrors++ColoumnErrors}.

validate_table_requirements(Row=#row{}, TableURI) ->
  LegalColoumns = global_interface:table_coloumns(TableURI),
  validate_legal_coloumns(LegalColoumns, Row).

validate_legal_coloumns(Coloumns, Row) -> validate_legal_coloumns(Coloumns, Row, {true, []}).
validate_legal_coloumns([First|Rest], Row, {SumValid, SumErrors}) ->
  {Valid, Errors} = validate_legal_coloumn(First, Row),
  validate_legal_coloumns(Rest, Row, {SumValid and Valid, Errors++SumErrors});
validate_legal_coloumns([], _, Result) -> Result.

validate_legal_coloumn(?COLOUMN_LABEL, #row{}) -> {true, []};
validate_legal_coloumn(LegalColoumn, #row{coloumns=Coloumns}) ->
  ColoumnURIs = [ColoumnURI || {ColoumnURI, _} <- Coloumns],
  case lists:member(LegalColoumn, ColoumnURIs) of
    true -> {true, []};
    false -> case global_interface:read_cell(?COLOUMN_OPTIONAL, LegalColoumn) of
      false -> {false, legal_coloumn_missing(LegalColoumn)};
      true -> {true, []}
    end
  end.

validate_coloumns(Row=#row{coloumns=Coloumns}) ->
  validate_coloumn_values(Coloumns, Row, {true, []}).

validate_coloumn_values([{ColoumnURI, Value}|Rest], Row, {Valid, Errors}) ->
  {ValidColoumn, ColoumnErrors} = case utils:row2coloumn(global_interface:read_row(ColoumnURI)) of
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

validate_coloumn_range(Range, ?ARITY_ONE, _, Value=#row{}, _Row) -> check(Value, Range);

validate_coloumn_range(Range, ?ARITY_ONE, _, RowURI=#row_uri{}, _Row) ->
  ValuesTable = utils:row_uri2table_uri(RowURI),
  {lists:member(Range, global_interface:flat_table_hierarchy(ValuesTable)), []}.

%Merges Error results
sum_result(Result) -> sum_result(Result, {true, []}).
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.

%Error messages
legal_coloumn_missing(LegalColoumn) -> [[{message, <<"Coloumn missing on row">>}, {value, LegalColoumn}]].
coloumn_not_exists(Coloumn, Value) ->
  [[{message, <<"Coloumn does not exist">>}, {coloumn, Coloumn}, {value, Value}]].
invalid_coloumn_value(Row=#row{uri=_URI}, Coloumn, Range, []) ->
  [[{message, <<"Invalid coloumn value">>}, {row, Row}, {range, Range}, {coloumn,Coloumn}]];
invalid_coloumn_value(Row=#row{uri=_URI}, Coloumn, Range, Details) ->
  [[{message, <<"Invalid coloumn value">>}, {row, Row}, {range, Range}, {coloumn,Coloumn},
    {details, Details}]].
arity_error() -> [[{message, <<"Arity is 'many' but only single value">>}]].