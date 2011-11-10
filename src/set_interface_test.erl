-module(set_interface_test).
-export([run/0]).

run() ->
  store_sample(),
  {ok, success}.

store_sample() ->
  Set = test_data:sample_set(),
  {atomic, {ok, success}} = t(fun() -> store_interface:write_item(Set) end).

t(Fun) ->
  store_interface:transaction(Fun).