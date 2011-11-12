-module(set_interface_test).
-export([run/0]).

run() ->
  store_sample(),
  test_set2record(),
  {ok, success}.

store_sample() ->
  Set = test_data:sample_set(),
  {atomic, {ok, success}} = t(fun() -> store_interface:write_item(Set) end).

test_set2record() ->
  SampleSet = test_data:sample_set(),
  RecordSet = test_data:record_set(),
  RecordSet == set_interface:set2records(SampleSet).

t(Fun) ->
  store_interface:transaction(Fun).