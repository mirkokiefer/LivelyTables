-module(store_test, [Store]).

-export([run/0]).

-include("../include/records.hrl").

run() ->
  MetaStore = local_stores:get(?META_DB),
  MetaStore:reset(),
  {atomic, {ok, success}} = MetaStore:transaction(fun() -> test_write_core(MetaStore) end),
  {atomic, {ok, success}} = Store:transaction(fun test_write_tables/0),
  {ok, success}.

test_write_core(MetaStore) ->
  {ok, success} = MetaStore:write(test_data:core_tables()),
  {ok, success} = MetaStore:write(test_data:core_coloumns()).

test_write_tables() ->
  {ok, success} = Store:write(test_data:tables()).