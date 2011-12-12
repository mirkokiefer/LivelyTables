%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(pluggable_store, [DBRows, DBRows2Table, DBTableIncludes]).

-export([init/0, reset/0, delete/0]).
-export([transaction/1, write/1, read_rows/1, read_rows_of_table/1, read_tables_of_row/1, read_parent_tables/1, read_child_tables/1]).

-define(DB_TABLES, [DBRows, DBRows2Table, DBTableIncludes]).

-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl" ).

init() -> create_tables().

create_tables() ->
  utils:log(mnesia:create_table(DBRows, [
    {attributes, record_info(fields, db_rows)},
    {record_name, db_rows},
    {type, set},
    {disc_copies,[node()]}
  ])),
  utils:log(mnesia:create_table(DBRows2Table, [
    {attributes, record_info(fields, db_rows2table)},
    {record_name, db_rows2table},
    {type, bag},
    {disc_copies,[node()]},
    {index, [table_id]}
  ])),
  utils:log(mnesia:create_table(DBTableIncludes, [
    {attributes, record_info(fields, db_table_includes)},
    {record_name, db_table_includes},
    {type, bag},
    {disc_copies,[node()]},
    {index, [included_table_id]}
  ])).

% deletes all table entries
reset() ->
  try mnesia:table_info(DBRows, type) of
    _Any -> [mnesia:clear_table(Tab) || Tab <- ?DB_TABLES]
  catch
    _:_ -> init()
  end.

delete() ->
 [mnesia:delete_table(Each) || Each <- ?DB_TABLES].

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
  F = fun() -> mnesia:index_read(DBRows2Table, TableURI, #db_rows2table.table_id) end,
  {atomic, Records} = mnesia:transaction(F),
  [Row || #db_rows2table{row_id=Row} <- Records].
  
read_child_tables(TableURI) ->
  F = fun() -> mnesia:index_read(DBTableIncludes, TableURI, #db_table_includes.included_table_id) end,
  {atomic, Records} = mnesia:transaction(F),
  [Subtable || #db_table_includes{table_id=Subtable} <- Records].

