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
  Result = [store_interface:write_property(Property) || Property <- test_properties()],
  check_each_result(Result).

test_write_invalid_items() ->
  Result = [store_interface:write_item(Item) || Item <- bad_items()],
  check_each_invalid_result(Result).

test_write_valid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_items()],
  check_each_result(Result).

test_update_valid_items() ->
  Result = [store_interface:write_item(Item) || Item <- test_items_updated()],
  check_each_result(Result).

test_update_invalid_items() ->
  Result = [store_interface:write_item(Item) || Item <- bad_items_updated()],
  check_each_invalid_result(Result).

test_composite_items() ->
  Result = [store_interface:write_item(Item) || Item <- composite_items()],
  check_each_result(Result).

test_items() ->
  Paul = #item{uri= <<"paul">>, label= <<"Paul">>, types=[<<"employee">>], properties=[
    {<<"age">>, 30},
    {<<"salary">>, 5000}
  ]},
  Jim = #item{uri= <<"jim">>, label= <<"Jim">>, types=[<<"manager">>], properties=[
    {<<"age">>, 40},
    {<<"salary">>, 10000},
    {<<"manages">>, [<<"paul">>]}
  ]},
  [Paul, Jim].

test_items_updated() ->
  UpdatedPaul = #item{uri= <<"paul">>, properties=[
    {<<"boss">>, <<"jim">>}
  ]},
  [UpdatedPaul].

test_properties() ->
  Manages = #property{uri= <<"manages">>, label= <<"Manages">>, range= <<"employee">>,
    arity=?ARITY_MANY, optional=true},
  Boss = #property{uri= <<"boss">>, label= <<"Boss">>, range= <<"manager">>, inverse= <<"manages">>, optional=true},
  Salary = #property{uri= <<"salary">>, label= <<"Salary">>, range=?PROPERTY_TYPE_NUMBER},
  Age = #property{uri= <<"age">>, label= <<"Age">>, range=?PROPERTY_TYPE_NUMBER},
  [Manages, Boss, Salary, Age].

bad_items() ->
  Paul = #item{uri= <<"paul">>, label= <<"Paul">>, types=[<<"employee">>], properties=[
    {<<"age">>, <<"40">>},
    {<<"salary">>, 5000},
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

bad_items_updated() ->
  Paul = #item{uri= <<"paul">>, properties=[
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

composite_items() ->
  Alex = #item{uri= <<"alex">>, label= <<"Alex">>, types=[<<"employee">>], properties=[
    {<<"age">>, 50},
    {<<"salary">>, 2000},
    {<<"boss">>, #item{uri= <<"jack">>, label= <<"Jack">>, types=[<<"manager">>], properties=[
      {<<"age">>, 40},
      {<<"salary">>, 10000}
    ]}}
  ]},
  Fred = #item{uri= <<"fred">>, label= <<"Fred">>, types=[<<"employee">>], properties=[
    {<<"age">>, 20},
    {<<"salary">>, 2500},
    {<<"boss">>, #item{label= <<"George">>, types=[<<"manager">>], properties=[
      {<<"age">>, 60},
      {<<"salary">>, 12000}
    ]}}
  ]},
  [Alex, Fred].

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