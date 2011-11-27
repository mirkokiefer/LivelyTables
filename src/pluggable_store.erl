%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(pluggable_store, [DBRows, DBRows2Table, DBTableIncludes]).

-export([start/0, stop/0, reset/0, clear/0]).
-export([transaction/1, write/1, read_rows/1, read_rows_of_table/1, read_tables_of_row/1, read_parent_tables/1, read_child_tables/1]).

-define(DB_TABLES, [DBRows, DBRows2Table, DBTableIncludes]).

-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl" ).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(),
  mnesia:stop().

create_tables() ->
  utils:log(mnesia:create_table(DBRows, [
    {attributes, record_info(fields, db_rows)},
    {record_name, db_rows},
    {type, set},
    {disc_copies,[node()]}
  ])),
  mnesia:create_table(DBRows2Table, [
    {attributes, record_info(fields, db_rows2table)},
    {record_name, db_rows2table},
    {type, bag},
    {disc_copies,[node()]},
    {index, [table]}
  ]),
  mnesia:create_table(DBTableIncludes, [
    {attributes, record_info(fields, db_table_includes)},
    {record_name, db_table_includes},
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
  [mnesia:clear_table(Tab) || Tab <- ?DB_TABLES].

start() ->
  mnesia:start(),
  mnesia:wait_for_tables(?DB_TABLES, 20000).

stop() ->
  mnesia:stop().

transaction(Fun) ->
  git:transaction(fun() -> mnesia:transaction(Fun) end).
  
write([First|Rest]) ->
  write(First),
  write(Rest);
write([]) -> {ok, success};

write(Record=#db_rows{}) -> mnesia:write(DBRows, Record, write);
write(Record=#db_rows2table{}) -> mnesia:write(DBRows2Table, Record, write);
write(Record=#db_table_includes{}) -> mnesia:write(DBTableIncludes, Record, write).

read(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

read_rows(ID) -> read(DBRows, ID).

read_tables_of_row(RowID) -> read(DBRows2Table, RowID).

read_parent_tables(TableID) -> read(DBTableIncludes, TableID).

read_rows_of_table(TableURI) ->
  F = fun() -> mnesia:index_read(DBRows2Table, TableURI, #db_rows2table.table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Row || #db_rows2table{row=Row} <- Records].
  
read_child_tables(TableURI) ->
  F = fun() -> mnesia:index_read(DBTableIncludes, TableURI, #db_table_includes.included_table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Subtable || #db_table_includes{table=Subtable} <- Records].

