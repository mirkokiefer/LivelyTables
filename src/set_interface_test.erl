-module(set_interface_test, [Store]).
-export([run/0]).

run() ->
  store_sample(),
  test_set2record(),
  {ok, success}.

store_sample() ->
  Set = test_data:sample_set(),
  {atomic, {ok, success}} = t(fun() -> Store:write_row(Set) end).

test_set2record() ->
  SampleSet = test_data:sample_set(),
  RecordSet = test_data:record_set(),
  SetUtils = set_utils:new(Store),
  RecordSet = SetUtils:set2records(SampleSet).

t(Fun) ->
  Store:transaction(Fun).