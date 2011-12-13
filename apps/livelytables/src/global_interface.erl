%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(global_interface).

-include("../include/records.hrl").

-export([read_dbs/1, read_tables/1, read_rows/1, read_row/1, read_cell/2]).
-export([write_row/1, write_table/1, write_column/1]).
-export([table_hierarchy/1, flat_table_hierarchy/1, table_columns/1]).

read_dbs(Domain) -> not_implemented.

read_tables(#db_uri{domain=?LOCALHOST, db=DB}) ->
  Store = local_stores:get_db(DB),
  [#row_uri{db=DB, table=?TABLE_ID, row=TableID} || TableID <- [?ROW_ID | Store:read_child_tables(?ROW_ID)]].

read_rows(#row_uri{domain=?LOCALHOST, db=DB, table=?TABLE_ID, row=TableID}) ->
  Store = local_stores:get_db(DB),
  [#row_uri{db=DB, table=TableID, row=RowID} || RowID <- Store:read_rows_of_table(TableID)];

read_rows(#row_uri{domain=Domain, db=DB, table=?TABLE_ID, row=Table}) ->
  http_get([Domain, DB, Table]).

read_row(URI=#row_uri{domain=?LOCALHOST, db=DB, table=TableID, row=RowID}) ->
  Store = local_stores:get_db(DB),
  case Store:in_table(RowID, TableID) of
    false -> undefined;
    true -> read_local_row(URI, Store)
  end;

read_row(#row_uri{domain=Domain, db=DB, table=Table, row=Row}) ->
  http_get([Domain, DB, Table, Row]).

read_local_row(URI=#row_uri{row=RowID}, Store) ->
  case Store:read_row(RowID) of
    undefined -> undefined;
    Row=#row{} ->
      #row{columns=Columns} = Row,
      LegalColumns = table_columns(utils:row_uri2table_uri(URI)),
      MissingCols = missing_columns(RowID, Columns, LegalColumns),
      FilteredColumns = [Column || Column={ColumnURI,_} <- Columns++MissingCols,
        lists:member(ColumnURI, LegalColumns)],
      Row#row{uri=URI, columns=lists:sort(FilteredColumns)}
  end.

read_cell(?COLUMN_PARENTS, #row_uri{domain=?LOCALHOST, db=DB, table=?TABLE_ID, row=RowID}) ->
  Store = local_stores:get_db(DB),
  Store:read_parent_tables(RowID);

read_cell(ColumnURI, RowURI=#row_uri{db=DB, row=RowID}) ->
  Store = local_stores:get_db(DB),
  utils:row_column(ColumnURI, Store:read_row(RowID)).

write_row(Row=#row{uri=#row_uri{domain=?LOCALHOST, db=DB, table=?TABLE_ID, row=RowID}}) ->
  Store = local_stores:get_db(DB),
  Table = utils:row2table(Row),
  Parents = case Table#table.parents of
    undefined -> [?ROW#row_uri{db=DB}];
    Any -> Any
  end,
  Store:transaction(fun() -> Store:write(Table#table{parents=Parents}) end);

write_row(Row=#row{uri=#row_uri{domain=?LOCALHOST, db=DB, table=TableID, row=RowID}}) ->
  Store = local_stores:get_db(DB),
  OldRow = Store:read_row(RowID),
  MergedRow = merge_rows(Row, OldRow),
  case validation:check(MergedRow) of
    {true, _} -> Store:transaction(fun() -> Store:write(MergedRow) end);
    {false, Errors} -> {error, Errors}
  end;

write_row(Row=#row{uri=#row_uri{domain=Domain, db=DB, table=Table, row=Row}}) ->
  http_put([Domain, DB, Table, Row], Row).

write_table(Table=#table{}) -> write_row(utils:table2row(Table)).

write_column(Column=#column{}) -> write_row(utils:column2row(Column)).

% utility functions

http_get(URI) -> not_implemented.

http_put(URI, Data) -> not_implemented.

table_hierarchy(TableURI=#row_uri{table=?TABLE_ID}) ->
  Parents = read_cell(?COLUMN_PARENTS, TableURI),
  [TableURI | [table_hierarchy(Parent) || Parent <- Parents]].

flat_table_hierarchy(TableURI=#row_uri{}) ->
  utils:set(lists:flatten(table_hierarchy(TableURI))).

table_columns(TableURI=#row_uri{}) ->
  Hierarchy = [TableURI|flat_table_hierarchy(TableURI)],
  utils:set(lists:flatten([read_cell(?COLUMN_LEGALCOLUMNS, Table) || Table <- Hierarchy])).

missing_columns(?ROW, _, _) -> [];
missing_columns(?TABLE, _, _) -> [];
missing_columns(_URI, ColumnValues, LegalColumns) ->
  Columns = columnvalues2columns(ColumnValues),
  MissingColumns = (LegalColumns -- Columns) -- [?COLUMN_LABEL],
  [{Column, undefined} || Column <- MissingColumns].

columnvalues2columns(ColumnValues) -> [Column || {Column, _} <- ColumnValues].

merge_rows(Row, undefined) -> Row;

merge_rows(NewRow=#row{uri=URI}, OldRow) ->
  TableURI = utils:row_uri2table_uri(URI),
  LegalColumns = table_columns(TableURI),
  #row{label=OldLabel, columns=OldColumns} = OldRow,
  #row{label=NewLabel, columns=NewColumns} = NewRow,
  MergedLabel = case NewLabel of
    undefined -> OldLabel;
    _ -> NewLabel
  end,
  MergedColumns = merge_columns(OldColumns, NewColumns, LegalColumns),
  NewRow#row{label=MergedLabel, columns=MergedColumns}.

merge_columns(OldColumns, NewColumns, LegalColumns) ->
  NewColumnURIs = [URI || {URI, _} <- NewColumns],
  LeftOutColumns = [Prop || Prop={URI,_} <- OldColumns,
    lists:member(URI, NewColumnURIs) == false,
    lists:member(URI, LegalColumns) == false
  ],
  LeftOutColumns ++ NewColumns.
