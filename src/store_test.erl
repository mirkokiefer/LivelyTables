-module(store_test, [Store]).

-export([run/0]).

-include("../include/records.hrl").

run() ->
  {atomic, {ok, success}} = Store:transaction(fun test_write_tables/0),
  {ok, success}.

test_write_tables() ->
  {ok, success} = Store:write(test_data:tables()).