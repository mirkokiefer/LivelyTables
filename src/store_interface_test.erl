-module(store_interface_test).
-export([run/0]).

-include("../include/records.hrl").

run() ->
  {atomic, {ok, success}} = t(fun test_write_properties/0),
  {atomic, {ok, success}} = t(fun test_write_invalid_items/0),
  {atomic, {ok, success}} = t(fun test_write_valid_items/0),
  {atomic, {ok, success}} = t(fun test_update_valid_items/0),
  {atomic, {ok, success}} = t(fun test_update_invalid_items/0),
  {atomic, {ok, success}} = t(fun test_composite_items/0),
  {ok, success}.

test_write_properties() ->
  Result = [store_interface:write_property(Property) || Property <- test_data:properties()],
  check_each_result(Result).

test_write_invalid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_data:invalid_items()],
  check_each_invalid_result(Result).

test_write_valid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_data:items()],
  check_each_result(Result).

test_update_valid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_data:items_updated()],
  check_each_result(Result).

test_update_invalid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_data:invalid_items_updated()],
  check_each_invalid_result(Result).

test_composite_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_data:composite_items2()],
  check_each_result(Result).

check_each_result(Result) ->
  case lists:all(fun(Each) -> Each == {ok, success} end, Result) of
    true -> {ok, success};
    false -> {error, Result}
  end.

check_each_invalid_result(Result) ->
  ReducedResult = lists:all(fun(Each) ->
    case Each of
      {error, _} -> true;
      _ -> false
    end
  end, Result),
  case ReducedResult of
    true -> {ok, success};
    false -> {error, Result}
  end.

t(Fun) ->
  store_interface:transaction(Fun).