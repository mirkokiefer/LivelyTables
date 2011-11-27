%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(store, [Store]).
-export([start/0, stop/0, reset/0, clear/0]).
-export([transaction/1, write_row/1, write_row/2, write_table/1, write_coloumn/1, write/2, write_all/1]).
-export([read_row/2, read_row/1, read_table/1, read_coloumn/1,
  read_rows_of_table/1, read_tables_of_row/1, read_direct_tables_of_row/1,
  read_parent_tables/1, read_direct_parent_tables/1, read_coloumns_of_table/1,
  read_child_tables/1, read_direct_child_tables/1]).

-include("../include/records.hrl").

start() -> Store:start().

stop() -> Store:stop().

% deletes and re-creates the schema
reset() -> Store:reset().

% deletes all table entries
clear() -> Store:clear().

transaction(Fun) -> Store:transaction(Fun).

write_all(Records) ->
  [write(Record) || Record <- Records],
  {ok, success}.

% write with validation
write_row(Row) -> write_row(Row, ?ROW).
 
write_row(Row=#row{}, Table) -> write(Row, Table).
 
write_table(Table=#table{}) -> write(utils:table2row(Table), ?TABLE).
 
write_coloumn(Coloumn=#coloumn{}) -> write(utils:coloumn2row(Coloumn), ?COLOUMN).

write(Row=#row{uri=URI}, Table) ->
  OldRow = read_row(URI),
  MergedRow = merge(Row, OldRow, Table),
  Validation = validation:new(?MODULE:new(Store)),
  case Validation:check(MergedRow) of
    {true, _} -> write_all([MergedRow]);
    {false, Errors} -> {error, Errors}
  end.

% write without validation
write(Row=#row{uri=URI, label=Label, tables=Tables, coloumns=Coloumns}) ->
  ResolvedRow = Row#row{tables=resolve(utils:set(Tables)), coloumns=resolve_coloumns(Coloumns)},
  #row{tables=ResolvedTables, coloumns=ResolvedColoumns} = ResolvedRow,
  RowTableRecord = #db_rows{uri=URI, label=Label, coloumns=ResolvedColoumns},
  RowTableTableRecords = [#db_rows2table{row=URI, table=Table} || Table <- ResolvedTables],
  TableSubTableRecords = db_table_includes_records(Row),
  git:write(ResolvedRow),
  Store:write([RowTableRecord] ++ RowTableTableRecords ++ TableSubTableRecords);

write(Table=#table{}) ->
  write(utils:table2row(Table));

write(Coloumn=#coloumn{}) ->
    write(utils:coloumn2row(Coloumn)).

db_table_includes_records(Row=#row{uri=URI}) ->
  case utils:row_coloumn(?COLOUMN_PARENTS, Row) of
    undefined -> [];
    Parents -> [#db_table_includes{table=URI, included_table=resolve(Parent)} || Parent <- Parents]
  end.

% resolve embedded rows to their URIs if they exist and store them separately
resolve([]) -> [];

resolve([First|Rest]) ->
  [resolve(First)|resolve(Rest)];

resolve(Row=#row{uri=undefined, tables=Tables, coloumns=Coloumns}) ->
  Row#row{tables=resolve(Tables), coloumns=resolve_coloumns(Coloumns)};

resolve(Row=#row{uri=URI}) ->
  write(Row),
  URI;

resolve(Coloumn=#coloumn{uri=URI}) ->
  write(Coloumn),
  URI;

resolve(Table=#table{uri=URI}) ->
  write(Table),
  URI;

resolve(URI) -> URI.

resolve_coloumns([{Coloumn, Value}|Rest]) ->
  [{Coloumn, resolve(Value)}|resolve_coloumns(Rest)];
resolve_coloumns([]) -> [].

% read with row integrity checking
read_row(RowURI, TableURI) ->
  Row = #row{coloumns=Coloumns} = read_row(RowURI),
  LegalColoumns = read_coloumns_of_table(TableURI),
  MissingCols = missing_coloumns(RowURI, Coloumns, LegalColoumns),
  FilteredColoumns = [Coloumn || Coloumn={URI,_} <- Coloumns++MissingCols, lists:member(URI, LegalColoumns)],
  Row#row{coloumns=lists:sort(FilteredColoumns)}.

% read raw row data
read_row(Row=#row{}) -> Row;

read_row(URI) ->
  case Store:read_rows(URI) of
    [#db_rows{uri=URI, label=Label, coloumns=Coloumns}] ->
      Tables = read_direct_tables_of_row(URI),
      #row{uri=URI, label=Label, tables=Tables, coloumns=Coloumns};
    [] -> undefined
  end.

% Table "Row" doesn't have included_tables so we need to implement it explicitly
read_table(?ROW) -> utils:row2table(read_row(?ROW));
read_table(URI) ->
  case {read_row(URI), read_direct_parent_tables(URI)} of
    {undefined, _} -> undefined;
    {_Row, []} -> undefined;
    {Row, _Parents} -> utils:row2table(Row)
  end.

read_coloumn(URI) ->
  case read_row(URI) of
    undefined -> undefined;
    Row -> utils:row2coloumn(Row)
  end.

read_direct_tables_of_row(RowURI) ->
  [Table || #db_rows2table{table=Table} <- Store:read_tables_of_row(RowURI)].

read_tables_of_row(RowURI) -> utils:set(read_tables_of_row_internal(RowURI)).

read_tables_of_row_internal(RowURI) ->
  Tables = read_direct_tables_of_row(RowURI),
  Tables ++ lists:flatten([read_parent_tables(Table) || Table <- Tables]).

read_rows_of_table(TableURI) ->
  lists:flatten([read_direct_rows_of_table(Each) || Each <- [TableURI|read_child_tables(TableURI)]]).

read_direct_rows_of_table(TableURI) -> Store:read_rows_of_table(TableURI).

read_parent_tables(TableURI) ->
  DirectParents = read_direct_parent_tables(TableURI),
  DirectParents ++ lists:flatten([read_parent_tables(Parent) || Parent <- DirectParents]).

read_direct_parent_tables(TableURI) ->
  [Parent || #db_table_includes{included_table=Parent} <- Store:read_parent_tables(TableURI)].

read_child_tables(TableURI) ->
  DirectSubtables = read_direct_child_tables(TableURI),
  DirectSubtables ++ lists:flatten([read_child_tables(Each) || Each <- DirectSubtables]).

read_direct_child_tables(TableURI) -> Store:read_child_tables(TableURI).

read_coloumns_of_table(TableURI) ->
  TableChain = [read_table(URI) || URI <- [TableURI | read_parent_tables(TableURI)]],
  lists:flatten([LegalColoumns || #table{legal_coloumns=LegalColoumns} <- TableChain]).

%utility functions
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
  #table{legal_coloumns=LegalColoumns} = read_table(NewTable),
  MergedColoumns = merge_coloumns(OldColoumns, NewColoumns, LegalColoumns),
  NewRow#row{label=MergedLabel, tables=MergedTables, coloumns=MergedColoumns}.

merge_coloumns(OldColoumns, NewColoumns, LegalColoumns) ->
  NewColoumnURIs = [URI || {URI, _} <- NewColoumns],
  LeftOutColoumns = [Prop || Prop={URI,_} <- OldColoumns,
    lists:member(URI, NewColoumnURIs) == false,
    lists:member(URI, LegalColoumns) == false
  ],
  LeftOutColoumns ++ NewColoumns.

missing_coloumns(?ROW, _, _) -> [];
missing_coloumns(?TABLE, _, _) -> [];
missing_coloumns(_URI, ColoumnValues, LegalColoumns) ->
  Coloumns = coloumnvalues2columns(ColoumnValues),
  MissingColoumns = (LegalColoumns -- Coloumns) -- [?COLOUMN_LABEL, ?COLOUMN_TABLES],
  [{Coloumn, undefined} || Coloumn <- MissingColoumns].

coloumnvalues2columns(ColumnValues) -> [Coloumn || {Coloumn, _} <- ColumnValues].
