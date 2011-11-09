-module(store_test).

-export([run/0]).

-include("../include/records.hrl").

run() ->
  {atomic, {ok, success}} = store:transaction(fun test_write_core/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_types/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_composite/0),
  {atomic, {ok, success}} = store:transaction(fun test_write_set_core/0),
  {ok, success}.

test_write_core() ->
  {ok, success} = store:write_all(test_data:core_types()),
  {ok, success} = store:write_all(test_data:core_properties()).

test_write_types() ->
  {ok, success} = store:write_all(test_data:types()).

test_write_composite() ->
  {ok, success} = store:write_all(test_data:composite_items()).

test_write_set_core() ->
  {ok, success} = store:write_all(test_data:set_properties()),
  {ok, success} = store:write_all(test_data:set_types()).