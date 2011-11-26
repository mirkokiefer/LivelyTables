%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(store, [Store]).
-export([start/0, stop/0, reset/0, clear/0]).
-export([transaction/1, write_all/1, read_row/1, read_table/1, read_coloumn/1,
  read_rows_of_table/1, read_tables_of_row/1, read_direct_tables_of_row/1,
  read_subtables/1, read_direct_subtables/1, read_coloumns_of_table/1,
  read_tables_including/1, read_tables_including_directly/1]).

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
  case {read_row(URI), read_direct_subtables(URI)} of
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
  Tables ++ lists:flatten([read_subtables(Table) || Table <- Tables]).

read_rows_of_table(TableURI) ->
  lists:flatten([read_direct_rows_of_table(Each) || Each <- [TableURI|read_tables_including(TableURI)]]).

read_direct_rows_of_table(TableURI) -> Store:read_rows_of_table(TableURI).

read_subtables(TableURI) ->
  DirectParents = read_direct_subtables(TableURI),
  DirectParents ++ lists:flatten([read_subtables(Parent) || Parent <- DirectParents]).

read_direct_subtables(TableURI) ->
  [Parent || #db_table_includes{included_table=Parent} <- Store:read_subtables(TableURI)].

read_tables_including(TableURI) ->
  DirectSubtables = read_tables_including_directly(TableURI),
  DirectSubtables ++ lists:flatten([read_tables_including(Each) || Each <- DirectSubtables]).

read_tables_including_directly(TableURI) -> Store:read_tables_including(TableURI).

read_coloumns_of_table(TableURI) ->
  TableChain = [read_table(URI) || URI <- [TableURI | read_subtables(TableURI)]],
  lists:flatten([LegalColoumns || #table{legal_coloumns=LegalColoumns} <- TableChain]).
