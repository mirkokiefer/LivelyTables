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
-export([read_row/1, read_table/1, read_coloumn/1]).
-export([tables_with_legal_coloumns/1, read_rows_of_table/1, read_tables_of_row/1,
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

write(Row=#row{uri=URI, label=Label, coloumns=Coloumns}) ->
  #row_uri{table=TableID, row=RowID} = URI,
  utils:log({write, StoreName, RowID}),
  DBRow = #db_rows{row_id=RowID, label=Label, coloumns=Coloumns},
  Rows2Table = #db_rows2table{row_id=RowID, table_id=TableID},
  TableIncludes = db_table_includes_records(RowID, Row),
  %git:write(Row),
  Store:write([DBRow, Rows2Table] ++ TableIncludes);

write(Table=#table{}) ->
  write(utils:table2row(Table));

write(Coloumn=#coloumn{}) ->
    write(utils:coloumn2row(Coloumn)).

db_table_includes_records(RowID, Row) ->
  case utils:row_coloumn(?COLOUMN_PARENTS, Row) of
    undefined -> [];
    Parents -> [#db_table_includes{table_id=RowID, included_table_id=Parent} || Parent <- Parents]
  end.

% Table "Row" doesn't have included_tables so we need to implement it explicitly
read_table(TableID) ->
  case read_row(TableID) of
    undefined -> undefined;
    Row -> utils:row2table(Row)
  end.

read_coloumn(ColoumnID) ->
  case read_row(ColoumnID) of
    undefined -> undefined;
    Row -> utils:row2coloumn(Row)
  end.

read_row(RowID) ->
  case Store:read_rows(RowID) of
    [#db_rows{label=Label, coloumns=Coloumns}] ->
      #row{label=Label, coloumns=Coloumns};
    [] -> undefined
  end.

read_tables_of_row(RowID) ->
  [TableID || #db_rows2table{table_id=TableID} <- Store:read_tables_of_row(RowID)].

read_rows_of_table(TableURI) ->
  lists:flatten([read_direct_rows_of_table(Each) || Each <- [TableURI|read_child_tables(TableURI)]]).

read_direct_rows_of_table(TableURI) -> Store:read_rows_of_table(TableURI).

read_parent_tables(TableID) ->
  [ParentID || #db_table_includes{included_table_id=ParentID} <- Store:read_parent_tables(TableID)].

read_child_tables(TableURI) ->
  DirectSubtables = read_direct_child_tables(TableURI),
  DirectSubtables ++ lists:flatten([read_child_tables(Each) || Each <- DirectSubtables]).

read_direct_child_tables(TableURI) -> Store:read_child_tables(TableURI).

%utility functions
tables_with_legal_coloumns(ValidLegalColoumns) ->
  tables_with_legal_coloumns(?ROW, ValidLegalColoumns).

tables_with_legal_coloumns(CurrentTableURI, ValidLegalColoumns) ->
  #table{legal_coloumns=LegalColoumns} = read_table(CurrentTableURI),
  case utils:is_subset(ValidLegalColoumns, LegalColoumns) of
    true -> [CurrentTableURI];
    false -> Subtables = read_direct_child_tables(CurrentTableURI),
      lists:flatten([tables_with_legal_coloumns(Subtable, ValidLegalColoumns) || Subtable <- Subtables])
  end.