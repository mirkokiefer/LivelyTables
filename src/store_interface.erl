-module(store_interface).

-export([transaction/1, write_row/1, write_row/2, write_table/1, write_coloumn/1,
  read_row/1, read_row/2, read_table/1, read_coloumn/1,
  read_rows_of_table/1, read_subtables/1, read_direct_subtables/1, read_parents/1,
  read_tables_of_row/1, read_legal_coloumns_of_table/1,
  validate/1]).

-include("../include/records.hrl").

transaction(Fun) ->
  store:transaction(Fun).

read_row(Row=#row{}) -> Row;

read_row(RowURI) -> store:read_row(RowURI).

read_row(RowURI, TableURI) ->
  Row = #row{coloumns=Coloumns} = store:read_row(RowURI),
  ParentTableURIs = [TableURI|store:read_parents(TableURI)],
  ParentTables = [store:read_table(URI) || URI <- ParentTableURIs],
  LegalProps = lists:flatten([Legal || #table{legal_coloumns=Legal} <- ParentTables]),
  FilteredProps = [Coloumn || Coloumn={URI,_} <- Coloumns, lists:member(URI, LegalProps)],
  Row#row{coloumns=FilteredProps}.

read_table(TableURI) -> store:read_table(TableURI).

read_coloumn(ColoumnURI) -> store:read_coloumn(ColoumnURI).

read_rows_of_table(TableURI) -> store:read_rows_of_table(TableURI).

read_subtables(TableURI) -> store:read_subtables(TableURI).

read_direct_subtables(TableURI) -> store:read_direct_subtables(TableURI).

read_parents(TableURI) -> store:read_parents(TableURI).

read_tables_of_row(RowURI) -> store:read_tables_of_row(RowURI).

read_legal_coloumns_of_table(TableURI) ->
  TableChain = [read_table(URI) || URI <- [TableURI | read_parents(TableURI)]],
  lists:flatten([LegalProps || #table{legal_coloumns=LegalProps} <- TableChain]).

write_row(Row) -> write_row(Row, ?ROW).

write_row(Row=#row{}, Table) -> write(Row, Table).

write_table(Table=#table{}) -> write(utils:table2row(Table), ?TABLE).

write_coloumn(Coloumn=#coloumn{}) -> write(utils:coloumn2row(Coloumn), ?COLOUMN).

write(Row=#row{uri=URI}, Table) ->
  OldRow = store:read_row(URI),
  MergedRow = merge(Row, OldRow, Table),
  case validate(MergedRow) of
    {true, _} -> case URI of
      undefined -> {false, legal_coloumn_missing(<<"uri">>)};
      _ -> store:write_all([MergedRow])
    end;
    {false, Errors} -> {error, Errors}
  end.

merge(Row=#row{tables=Tables}, undefined, TableURI) ->
  case lists:member(TableURI, Tables) of
      true -> Row;
      false -> Row#row{tables=[TableURI|Tables]}
  end;

merge(Row, OldRow, TableURI) -> merge_rows(Row, OldRow, TableURI).

merge_rows(NewRow, OldRow, NewTable) ->
  #row{label=OldLabel, tables=OldTables, coloumns=OldColoumns} = OldRow,
  #row{label=NewLabel, tables=NewTables, coloumns=NewColoumns} = NewRow,
  MergedLabel = case NewLabel of
    undefined -> OldLabel;
    _ -> NewLabel
  end,
  MergedTables = case NewTables of
    [] -> [NewTable|OldTables];
    _Any -> case lists:member(NewTable, NewTables) of
      true -> NewTables;
      false -> [NewTable|NewTables]
    end
  end,
  #table{legal_coloumns=LegalColoumns} = store:read_table(NewTable),
  MergedColoumns = merge_coloumns(OldColoumns, NewColoumns, LegalColoumns),
  NewRow#row{label=MergedLabel, tables=MergedTables, coloumns=MergedColoumns}.

merge_coloumns(OldColoumns, NewColoumns, LegalColoumns) ->
  NewColoumnURIs = [URI || {URI, _} <- NewColoumns],
  LeftOutProps = [Prop || Prop={URI,_} <- OldColoumns,
    lists:member(URI, NewColoumnURIs) == false,
    lists:member(URI, LegalColoumns) == false
  ],
  LeftOutProps ++ NewColoumns.

validate(Table = #table{}) -> validate_row(utils:table2row(Table));
validate(Coloumn = #coloumn{}) -> validate_row(utils:coloumn2row(Coloumn));
validate(Row = #row{}) -> validate_row(Row).

validate_row(Row) ->
  {ValidTable, TableErrors} = validate_table_requirements(Row),
  {ValidColoumns, ColoumnErrors} = validate_coloumns(Row),
  {ValidTable and ValidColoumns, TableErrors++ColoumnErrors}.

validate_table_requirements(Row=#row{tables=Tables}) ->
  ParentTableURIs = utils:set(Tables ++ lists:flatten([store:read_parents(Table) || Table <- Tables])),
  ParentTables = [store:read_table(Each) || Each <- ParentTableURIs],
  Results = [validate_legal_coloumns(LegalProps, Row) || #table{legal_coloumns=LegalProps} <- ParentTables],
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
    false -> case store:read_coloumn(LegalColoumn) of
      #coloumn{optional=false} -> {false, legal_coloumn_missing(LegalColoumn)};
      #coloumn{optional=true} -> {true, []}
    end
  end.

validate_coloumns(#row{tables=Tables, coloumns=Coloumns}) ->
  {TablesExist, TableErrors} = case Tables of
    [] -> {false, legal_coloumn_missing(?COLOUMN_TABLES)};
    _ -> {true, []}
  end,
  {ValuesValid, ValuesErrors} = validate_coloumn_values(Coloumns),
  {TablesExist and ValuesValid, TableErrors ++ ValuesErrors}.

validate_coloumn_values(Coloumns) -> validate_coloumn_values(Coloumns, {true, []}).

validate_coloumn_values([{ColoumnURI, Value}|Rest], {Valid, Errors}) ->
  {ValidColoumn, ColoumnErrors} = case store:read_coloumn(ColoumnURI) of
    undefined -> {false, coloumn_not_exists(ColoumnURI, Value)};
    #coloumn{range=Range, arity=Arity} -> 
      Result = validate_coloumn_range(Range, Arity, Value),
      case Result of
        {true, _} -> {true, []};
        {false, RangeErrors} -> {false, invalid_coloumn_value(ColoumnURI, Range, RangeErrors)}
      end
  end,
  validate_coloumn_values(Rest, {Valid and ValidColoumn, ColoumnErrors ++ Errors});
validate_coloumn_values([], Result) -> Result.

validate_coloumn_range(_Range, ?ARITY_MANY, []) -> {true, []};
validate_coloumn_range(Range, ?ARITY_MANY, Values=[_|_]) ->
  Results = [validate_coloumn_range(Range, ?ARITY_ONE, Value) || Value <- Values],
  sum_result(Results);
validate_coloumn_range(_Range, ?ARITY_MANY, _Value) -> {false, arity_error()};
validate_coloumn_range(?COLOUMN_TYPE_STRING, ?ARITY_ONE, Value) -> {is_bitstring(Value), []};
validate_coloumn_range(?COLOUMN_TYPE_NUMBER, ?ARITY_ONE, Value) -> {is_number(Value), []};
validate_coloumn_range(?COLOUMN_TYPE_BOOLEAN, ?ARITY_ONE, Value) -> {is_boolean(Value), []};
validate_coloumn_range(_, ?ARITY_ONE, ?COLOUMN_TYPE_NUMBER) -> {true, []};
validate_coloumn_range(_, ?ARITY_ONE, ?COLOUMN_TYPE_STRING) -> {true, []};
validate_coloumn_range(_, ?ARITY_ONE, ?COLOUMN_TYPE_BOOLEAN) -> {true, []};

validate_coloumn_range(Range, ?ARITY_ONE, Value=#table{}) ->
  validate_coloumn_range(Range, ?ARITY_ONE, utils:table2row(Value));

validate_coloumn_range(Range, ?ARITY_ONE, Value=#coloumn{}) ->
  validate_coloumn_range(Range, ?ARITY_ONE, utils:coloumn2row(Value));

validate_coloumn_range(Range, ?ARITY_ONE, Value=#row{tables=Tables}) ->
  case validate_row(Value) of
    {true, _} ->
      Parents = Tables ++ lists:flatten([store:read_parents(Table) || Table <- Tables]),
      {lists:member(Range, Parents), []};
    {false, Errors} -> {false, Errors}
  end;
validate_coloumn_range(Range, ?ARITY_ONE, Value) ->
  {lists:member(Range, store:read_tables_of_row(Value)), []}.

%Merges Error results
sum_result(Result) -> sum_result(Result, {true, []}).
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.

%Error messages
legal_coloumn_missing(LegalColoumn) -> [[{message, <<"Coloumn missing on row">>}, {value, LegalColoumn}]].
coloumn_not_exists(Coloumn, Value) ->
  [[{message, <<"Coloumn does not exist">>}, {coloumn, Coloumn}, {value, Value}]].
invalid_coloumn_value(Coloumn, Range, []) ->
  [[{message, <<"Invalid coloumn value">>}, {range, Range}, {coloumn,Coloumn}]];
invalid_coloumn_value(Coloumn, Range, Details) ->
  [[{message, <<"Invalid coloumn value">>}, {range, Range}, {coloumn,Coloumn},
    {details, Details}]].
arity_error() -> [[{message, <<"Arity is 'many' but only single value">>}]].