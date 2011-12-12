-module(store_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/records.hrl").

write_tables_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun() ->
    Store = local_stores:get_db(?TEST_DB),
    ?assertMatch({atomic, {ok, success}}, write_tables(Store))
  end}.

write_tables(Store) ->
  Store:transaction(fun() -> Store:write(test_data:tables()) end).

setup() ->
  setup:ensure(),
  local_stores:create_db(?TEST_DB).
  
cleanup(Store) ->
  Store:delete().