-module(store_interface_test, [StoreInterface]).
-export([run/0]).

-include("../include/records.hrl").

run() ->
  test_validate_core(),
  test_validate_sets(),
  {atomic, {ok, success}} = t(fun test_write_coloumns/0),
  {atomic, {ok, success}} = t(fun test_write_invalid_rows/0),
  {atomic, {ok, success}} = t(fun test_write_valid_rows/0),
  {atomic, {ok, success}} = t(fun test_update_valid_rows/0),
  {atomic, {ok, success}} = t(fun test_update_invalid_rows/0),
  {atomic, {ok, success}} = t(fun test_composite_rows/0),
  {ok, success}.

test_validate_core() ->
  {ok, success} = test_validate(test_data:core_tables()),
  {ok, success} = test_validate(test_data:core_coloumns()).

test_validate_sets() ->
  {ok, success} = test_validate(test_data:set_coloumns()),
  {ok, success} = test_validate(test_data:set_tables()).

test_validate(Rows) ->
  Result = [StoreInterface:validate(Each) || Each <- Rows],
  check_each_valid(Result).

test_write_coloumns() ->
  Result = [StoreInterface:write_coloumn(Coloumn) || Coloumn <- test_data:coloumns()],
  check_each_result(Result).

test_write_invalid_rows() ->
  Result = [StoreInterface:write_row(Row) || Row <- test_data:invalid_rows()],
  check_each_invalid_result(Result).

test_write_valid_rows() ->
  Result = [StoreInterface:write_row(Row) || Row <- test_data:rows()],
  check_each_result(Result).

test_update_valid_rows() ->
  Result = [StoreInterface:write_row(Row) || Row <- test_data:rows_updated()],
  check_each_result(Result).

test_update_invalid_rows() ->
  Result = [StoreInterface:write_row(Row) || Row <- test_data:invalid_rows_updated()],
  check_each_invalid_result(Result).

test_composite_rows() ->
  Result = [StoreInterface:write_row(Row) || Row <- test_data:composite_rows2()],
  check_each_result(Result).

check_each_result(Result) ->
  case lists:all(fun(Each) -> Each == {ok, success} end, Result) of
    true -> {ok, success};
    false -> {error, Result}
  end.

check_each_valid(Result) ->
  case lists:all(fun(Each) -> Each == {true, []} end, Result) of
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
  StoreInterface:transaction(Fun).