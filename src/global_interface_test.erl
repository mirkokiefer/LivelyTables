-module(global_interface_test).
-export([run/0]).

-include("../include/records.hrl").

run() ->
  test_validate_core(),
  {ok, success} = test_write_tables(),
  {ok, success} = test_write_coloumns(),
  {ok, success} = test_write_invalid_rows(),
  {ok, success} = test_write_valid_rows(),
  {ok, success} = test_update_valid_rows(),
  {ok, success} = test_update_invalid_rows(),
  {ok, success}.

test_validate_core() ->
  {ok, success} = test_validate(bootstrap:meta_tables()),
  {ok, success} = test_validate(bootstrap:meta_coloumns()).

test_validate(Rows) ->
  Result = [validation:check(Each) || Each <- Rows],
  check_each_valid(Result).

test_write_tables() ->
  Result = [global_interface:write_table(Each) || Each <- test_data:tables()],
  check_each_result(Result).

test_write_coloumns() ->
  Result = [global_interface:write_coloumn(Coloumn) || Coloumn <- test_data:coloumns()],
  check_each_result(Result).

test_write_invalid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:invalid_rows()],
  check_each_invalid_result(Result).

test_write_valid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:rows()],
  check_each_result(Result).

test_update_valid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:rows_updated()],
  check_each_result(Result).

test_update_invalid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:invalid_rows_updated()],
  check_each_invalid_result(Result).

check_each_result(Result) ->
  case lists:all(fun(Each) -> Each == {atomic, {ok, success}} end, Result) of
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