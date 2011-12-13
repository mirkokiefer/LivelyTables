-module(global_interface_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/records.hrl").

validate_test_() ->
  {setup, fun setup/0, fun cleanup/1, {inorder, [
    validate_core()
  ]}}.

global_interface_test_() ->
  {setup, fun setup/0, fun cleanup/1, {inorder, [
    ?_assertMatch({ok, success}, write_tables()),
    ?_assertMatch({ok, success}, write_columns()),
    ?_assertMatch({ok, success}, write_invalid_rows()),
    ?_assertMatch({ok, success}, write_valid_rows()),
    ?_assertMatch({ok, success}, update_valid_rows()),
    ?_assertMatch({ok, success}, update_invalid_rows())
  ]}}.

validate_core() -> [
    ?_assertMatch({ok, success}, validate(setup:meta_tables())),
    ?_assertMatch({ok, success}, validate(setup:meta_columns()))
  ].

validate(Rows) ->
  Result = [validation:check(Each) || Each <- Rows],
  check_each_valid(Result).

write_tables() ->
  Result = [global_interface:write_table(Each) || Each <- test_data:tables()],
  check_each_result(Result).

write_columns() ->
  Result = [global_interface:write_column(Column) || Column <- test_data:columns()],
  check_each_result(Result).

write_invalid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:invalid_rows()],
  check_each_invalid_result(Result).

write_valid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:rows()],
  check_each_result(Result).

update_valid_rows() ->
  Result = [global_interface:write_row(Row) || Row <- test_data:rows_updated()],
  check_each_result(Result).

update_invalid_rows() ->
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
  
setup() ->
  setup:ensure(),
  local_stores:create_db(?TEST_DB).
  
cleanup(Store) ->
  Store:delete().