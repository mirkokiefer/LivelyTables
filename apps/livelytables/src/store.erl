%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(store, [StoreName, Store]).
-export([init/0, reset/0, delete/0, name/0]).
-export([transaction/1, write/1]).
-export([read_row/1, read_table/1, read_column/1]).
-export([tables_with_legal_columns/1, read_rows_of_table/1, read_tables_of_row/1, in_table/2,
  read_parent_tables/1, read_child_tables/1, read_direct_child_tables/1]).

-include("../include/records.hrl").

init() -> Store:init().

% deletes all table entries
reset() -> Store:reset().

delete() -> Store:delete().

name() -> StoreName.

transaction(Fun) -> Store:transaction(Fun).

% write without validation
write([First|Rest]) ->
  write(First),
  write(Rest);

write([]) -> {ok, success};

write(Row=#row{uri=URI, label=Label, columns=Columns}) ->
  #row_uri{table=TableID, row=RowID} = URI,
  DBRow = #db_rows{row_id=RowID, label=Label, columns=Columns},
  Rows2Table = #db_rows2table{row_id=RowID, table_id=TableID},
  TableIncludes = db_table_includes_records(RowID, Row),
  %git:write(Row),
  Store:write([DBRow, Rows2Table] ++ TableIncludes);

write(Table=#table{}) ->
  write(utils:table2row(Table));

write(Column=#column{}) ->
    write(utils:column2row(Column)).

db_table_includes_records(RowID, Row) ->
  case utils:row_column(?COLUMN_PARENTS, Row) of
    undefined -> [];
    Parents -> [#db_table_includes{table_id=RowID, included_table_id=ParentURI} || ParentURI <- Parents]
  end.

% Table "Row" doesn't have included_tables so we need to implement it explicitly
read_table(TableID) ->
  case read_row(TableID) of
    undefined -> undefined;
    Row -> utils:row2table(Row)
  end.

read_column(ColumnID) ->
  case read_row(ColumnID) of
    undefined -> undefined;
    Row -> utils:row2column(Row)
  end.

read_row(RowID) ->
  case Store:read_rows(RowID) of
    [#db_rows{label=Label, columns=Columns}] ->
      #row{label=Label, columns=Columns};
    [] -> undefined
  end.

read_tables_of_row(RowID) ->
  [TableID || #db_rows2table{table_id=TableID} <- Store:read_tables_of_row(RowID)].

read_rows_of_table(TableID) ->
  lists:flatten([read_direct_rows_of_table(Each) || Each <- [TableID|read_child_tables(TableID)]]).

read_direct_rows_of_table(TableURI) -> Store:read_rows_of_table(TableURI).

in_table(RowID, LegalTableID) ->
  Tables = read_tables_of_row(RowID),
  in_table_internal(Tables, LegalTableID).
  
in_table_internal(TableIDs, LegalTableID) ->
  case lists:member(LegalTableID, TableIDs) of
    true -> true;
    false -> lists:any(fun(TableID) ->
      in_table_internal(read_local_parent_tables(TableID), LegalTableID)
    end, TableIDs)
  end.

read_local_parent_tables(TableID) ->
  [ParentID || #row_uri{db=DB, row=ParentID} <- read_parent_tables(TableID), DB == StoreName].

read_parent_tables(TableID) ->
  [ParentID || #db_table_includes{included_table_id=ParentID} <- Store:read_parent_tables(TableID)].

read_child_tables(TableID) ->
  DirectSubtables = read_direct_child_tables(TableID),
  DirectSubtables ++ lists:flatten([read_child_tables(Each) || Each <- DirectSubtables]).

read_direct_child_tables(TableID) -> Store:read_child_tables(#row_uri{db=StoreName, table=?TABLE_ID, row=TableID}).

%utility functions
tables_with_legal_columns(ValidLegalColumns) ->
  tables_with_legal_columns(?ROW, ValidLegalColumns).

tables_with_legal_columns(CurrentTableURI, ValidLegalColumns) ->
  #table{legal_columns=LegalColumns} = read_table(CurrentTableURI),
  case utils:is_subset(ValidLegalColumns, LegalColumns) of
    true -> [CurrentTableURI];
    false -> Subtables = read_direct_child_tables(CurrentTableURI),
      lists:flatten([tables_with_legal_columns(Subtable, ValidLegalColumns) || Subtable <- Subtables])
  end.