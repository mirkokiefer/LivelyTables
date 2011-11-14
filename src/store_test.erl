-module(store_test).

-export([run/0]).

-include("../include/records.hrl").

run() ->
  {atomic, {ok, success}} = store:transaction(fun test_write_core/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_tables/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_composite/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_set_core/0),
  {ok, success}.

test_write_core() ->
  {ok, success} = store:write_all(test_data:core_tables()),
  {ok, success} = store:write_all(test_data:core_coloumns()).

test_write_tables() ->
  {ok, success} = store:write_all(test_data:tables()).

test_write_composite() ->
  {ok, success} = store:write_all(test_data:composite_rows()).

test_write_set_core() ->
  {ok, success} = store:write_all(test_data:set_coloumns()),
  {ok, success} = store:write_all(test_data:set_tables()).