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
-export([write_row/1, write_table/1, write_coloumn/1]).
-export([table_hierarchy/1, flat_table_hierarchy/1, table_coloumns/1]).

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
  case Store:read_row(RowID) of
    undefined -> undefined;
    Row=#row{} ->
      #row{coloumns=Coloumns} = Row,
      LegalColoumns = table_coloumns(utils:row_uri2table_uri(URI)),
      MissingCols = missing_coloumns(RowID, Coloumns, LegalColoumns),
      FilteredColoumns = [Coloumn || Coloumn={ColoumnURI,_} <- Coloumns++MissingCols,
        lists:member(ColoumnURI, LegalColoumns)],
      Row#row{uri=URI, coloumns=lists:sort(FilteredColoumns)}
  end;

read_row(#row_uri{domain=Domain, db=DB, table=Table, row=Row}) ->
  http_get([Domain, DB, Table, Row]).

read_cell(?COLOUMN_PARENTS, #row_uri{domain=?LOCALHOST, db=DB, table=?TABLE_ID, row=RowID}) ->
  Store = local_stores:get_db(DB),
  Store:read_parent_tables(RowID);

read_cell(ColoumnURI, RowURI=#row_uri{db=DB, row=RowID}) ->
  Store = local_stores:get_db(DB),
  utils:row_coloumn(ColoumnURI, Store:read_row(RowID)).

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

write_coloumn(Coloumn=#coloumn{}) -> write_row(utils:coloumn2row(Coloumn)).

% utility functions

http_get(URI) -> not_implemented.

http_put(URI, Data) -> not_implemented.

table_hierarchy(TableURI=#row_uri{table=?TABLE_ID}) ->
  Parents = read_cell(?COLOUMN_PARENTS, TableURI),
  [TableURI | [table_hierarchy(Parent) || Parent <- Parents]].

flat_table_hierarchy(TableURI=#row_uri{}) ->
  utils:set(lists:flatten(table_hierarchy(TableURI))).

table_coloumns(TableURI=#row_uri{}) ->
  Hierarchy = [TableURI|flat_table_hierarchy(TableURI)],
  utils:set(lists:flatten([read_cell(?COLOUMN_LEGALCOLOUMNS, Table) || Table <- Hierarchy])).

missing_coloumns(?ROW, _, _) -> [];
missing_coloumns(?TABLE, _, _) -> [];
missing_coloumns(_URI, ColoumnValues, LegalColoumns) ->
  Coloumns = coloumnvalues2columns(ColoumnValues),
  MissingColoumns = (LegalColoumns -- Coloumns) -- [?COLOUMN_LABEL],
  [{Coloumn, undefined} || Coloumn <- MissingColoumns].

coloumnvalues2columns(ColumnValues) -> [Coloumn || {Coloumn, _} <- ColumnValues].

merge_rows(Row, undefined) -> Row;

merge_rows(NewRow=#row{uri=URI}, OldRow) ->
  TableURI = utils:row_uri2table_uri(URI),
  LegalColoumns = table_coloumns(TableURI),
  #row{label=OldLabel, coloumns=OldColoumns} = OldRow,
  #row{label=NewLabel, coloumns=NewColoumns} = NewRow,
  MergedLabel = case NewLabel of
    undefined -> OldLabel;
    _ -> NewLabel
  end,
  MergedColoumns = merge_coloumns(OldColoumns, NewColoumns, LegalColoumns),
  NewRow#row{label=MergedLabel, coloumns=MergedColoumns}.

merge_coloumns(OldColoumns, NewColoumns, LegalColoumns) ->
  NewColoumnURIs = [URI || {URI, _} <- NewColoumns],
  LeftOutColoumns = [Prop || Prop={URI,_} <- OldColoumns,
    lists:member(URI, NewColoumnURIs) == false,
    lists:member(URI, LegalColoumns) == false
  ],
  LeftOutColoumns ++ NewColoumns.
