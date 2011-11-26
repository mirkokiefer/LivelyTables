-module(store_test, [Store]).

-export([run/0]).

-include("../include/records.hrl").

run() ->
  io:format("~p~n", [Store]),
  {atomic, {ok, success}} = Store:transaction(fun test_write_core/0),
  {atomic, {ok, success}} = Store:transaction(fun test_write_tables/0),
  {atomic, {ok, success}} = Store:transaction(fun test_write_composite/0),
  {atomic, {ok, success}} = Store:transaction(fun test_write_set_core/0),
  {ok, success}.

test_write_core() ->
  {ok, success} = Store:write_all(test_data:core_tables()),
  {ok, success} = Store:write_all(test_data:core_coloumns()).

test_write_tables() ->
  {ok, success} = Store:write_all(test_data:tables()).

test_write_composite() ->
  {ok, success} = Store:write_all(test_data:composite_rows()).

test_write_set_core() ->
  {ok, success} = Store:write_all(test_data:set_coloumns()),
  {ok, success} = Store:write_all(test_data:set_tables()).