-module(store_test, [Store]).

-export([run/0]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/records.hrl").

write_tables_test() ->
  Store = setup(),
  ?_assertMatch({ok, success}, write_tables(Store)),
  cleanup(Store).

write_tables(Store) ->
  Store:transaction(fun() -> Store:write(test_data:tables()) end).

setup() ->
  local_stores:create_db(?TEST_DB).
  
cleanup(Store) ->
  Store:delete().