
-module(store).
-export([init/0, reset/0, start/0, stop/0, clear/0]).
-export([transaction/1, write_all/1, read_row/1, read_table/1, read_coloumn/1,
  read_rows_of_table/1, read_tables_of_row/1, read_direct_tables_of_row/1,
  read_subtables/1, read_direct_subtables/1,
  read_tables_including/1, read_tables_including_directly/1]).

-include("../include/records.hrl").

-include_lib("stdlib/include/qlc.hrl" ).

-define(DB_PATH, "../output/tuples.tab").

-define(TABLES, [rows, rows2table, table_includes]).
-record(rows, {uri, label, coloumns=[]}).
-record(rows2table, {row, table}).
-record(table_includes, {table, included_table}).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(),
  mnesia:stop().

create_tables() ->
  utils:log(mnesia:create_table(rows, [
    {attributes, record_info(fields, rows)},
    {type, set},
    {disc_copies,[node()]}
  ])),
  mnesia:create_table(rows2table, [
    {attributes, record_info(fields, rows2table)},
    {type, bag},
    {disc_copies,[node()]},
    {index, [table]}
  ]),
  mnesia:create_table(table_includes, [
    {attributes, record_info(fields, table_includes)},
    {type, bag},
    {disc_copies,[node()]},
    {index, [included_table]}
  ]).

% deletes and re-creates the schema
reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  init(),
  start().

% deletes all table entries
clear() ->
  [mnesia:clear_table(Tab) || Tab <- ?TABLES].

start() ->
  mnesia:start(),
  mnesia:wait_for_tables(?TABLES, 20000).

stop() ->
  mnesia:stop().

transaction(Fun) ->
  git:transaction(fun() -> mnesia:transaction(Fun) end).

write_all(Records) ->
  [write(Record) || Record <- Records],
  {ok, success}.

write(Row=#row{uri=URI, label=Label, tables=Tables, coloumns=Coloumns}) ->
  ResolvedRow = Row#row{tables=resolve(utils:set(Tables)), coloumns=resolve_coloumns(lists:sort(Coloumns))},
  #row{tables=ResolvedTables, coloumns=ResolvedColoumns} = ResolvedRow,
  RowTableRecord = #rows{uri=URI, label=Label, coloumns=ResolvedColoumns},
  RowTableTableRecords = [#rows2table{row=URI, table=Table} || Table <- ResolvedTables],
  TableSubTableRecords = table_includes_records(Row),
  git:write(ResolvedRow),
  store_table_records([RowTableRecord] ++ RowTableTableRecords ++ TableSubTableRecords);

write(Table=#table{}) ->
  write(utils:table2row(Table));

write(Coloumn=#coloumn{}) ->
    write(utils:coloumn2row(Coloumn)).

table_includes_records(Row=#row{uri=URI}) ->
  case utils:row_coloumn(?COLOUMN_PARENTS, Row) of
    undefined -> [];
    Parents -> [#table_includes{table=URI, included_table=resolve(Parent)} || Parent <- Parents]
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

store_table_records([First|Rest]) ->
  mnesia:write(First),
  store_table_records(Rest);
store_table_records([]) -> {ok, success}.

read_row(URI) ->
  case read(rows, URI) of
    [#rows{uri=URI, label=Label, coloumns=Coloumns}] ->
      Tables = [Table || #rows2table{table=Table} <- read(rows2table, URI)],
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
  [Table || #rows2table{table=Table} <- read(rows2table, RowURI)].

read_tables_of_row(RowURI) -> utils:set(read_tables_of_row_internal(RowURI)).

read_tables_of_row_internal(RowURI) ->
  Tables = read_direct_tables_of_row(RowURI),
  Tables ++ lists:flatten([read_subtables(Table) || Table <- Tables]).

read_rows_of_table(TableURI) ->
  lists:flatten([read_direct_rows_of_table(Each) || Each <- [TableURI|read_tables_including(TableURI)]]).

read_direct_rows_of_table(TableURI) ->
  F = fun() -> mnesia:index_read(rows2table, TableURI, #rows2table.table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Row || #rows2table{row=Row} <- Records].

read_subtables(TableURI) ->
  DirectParents = read_direct_subtables(TableURI),
  DirectParents ++ lists:flatten([read_subtables(Parent) || Parent <- DirectParents]).

read_direct_subtables(TableURI) ->
  [Parent || #table_includes{included_table=Parent} <- read(table_includes, TableURI)].

read_tables_including(TableURI) ->
  DirectSubtables = read_tables_including_directly(TableURI),
  DirectSubtables ++ lists:flatten([read_tables_including(Each) || Each <- DirectSubtables]).

read_tables_including_directly(TableURI) ->
  F = fun() -> mnesia:index_read(table_includes, TableURI, #table_includes.included_table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Subtable || #table_includes{table=Subtable} <- Records].

read(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

query_row(URI) ->
  do(qlc:q([Row || Row=#row{uri=U} <- mnesia:table(row), U==URI])).

% utility functions
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
