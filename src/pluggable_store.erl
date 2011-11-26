%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(pluggable_store).

-export([start/0, stop/0, reset/0, clear/0]).
-export([transaction/1, write/1, read/2, read_rows_of_table/1, read_included_tables/1]).

-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl" ).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(),
  mnesia:stop().

create_tables() ->
  utils:log(mnesia:create_table(db_rows, [
    {attributes, record_info(fields, db_rows)},
    {type, set},
    {disc_copies,[node()]}
  ])),
  mnesia:create_table(db_rows2table, [
    {attributes, record_info(fields, db_rows2table)},
    {type, bag},
    {disc_copies,[node()]},
    {index, [table]}
  ]),
  mnesia:create_table(db_table_includes, [
    {attributes, record_info(fields, db_table_includes)},
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
  mnesia:write(First),
  write(Rest);
write([]) -> {ok, success}.

read(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

read_rows_of_table(TableURI) ->
  F = fun() -> mnesia:index_read(db_rows2table, TableURI, #db_rows2table.table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Row || #db_rows2table{row=Row} <- Records].
  
read_included_tables(TableURI) ->
  F = fun() -> mnesia:index_read(db_table_includes, TableURI, #db_table_includes.included_table) end,
  {atomic, Records} = mnesia:transaction(F),
  [Subtable || #db_table_includes{table=Subtable} <- Records].
  
%query_row(URI) ->
%  do(qlc:q([Row || Row=#row{uri=U} <- mnesia:table(row), U==URI])).

% utility functions
%do(Q) ->
%  F = fun() -> qlc:e(Q) end,
%  {atomic, Val} = mnesia:transaction(F),
%  Val.
