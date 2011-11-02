-module(store_test).

-export([test/0]).

-include("../include/records.hrl").

test() ->
  test_item_write(),
  test_type_write(),
  test_property_write(),
  test_item_read(),
  test_type_read(),
  test_property_read(),
  {ok, success}.

test_item_write() ->
  {ok, success} = store:write_all(test_items()).

test_type_write() ->
  {ok, success} = store:write_all(test_types()).

test_property_write() ->
  {ok, success} = store:write_all(test_properties()).

test_item_read() ->
  [First|_] = test_items(),
  First = store:read_item(First#item.uri).

test_type_read() ->
  [First|_] = test_types(),
  First = store:read_type(First#type.uri).

test_property_read() ->
  [First|_] = test_properties(),
  First = store:read_property(First#property.uri).

test_types() ->
  Person = #type{uri= <<"person">>, label= <<"Person">>, legal_properties=[<<"age">>]},
  Employee = #type{uri= <<"employee">>, label= <<"Employee">>, parents=[<<"person">>],
    legal_properties=[<<"salary">>, <<"boss">>]},
  Manager = #type{uri= <<"manager">>, label= <<"Manager">>, parents=[<<"employee">>],
    legal_properties=[<<"manages">>]},
  [Person, Employee, Manager].

test_items() ->
  Paul = #item{uri= <<"paul">>, label= <<"Paul">>, types=[<<"employee">>], properties=[
    {<<"age">>, 30},
    {<<"salary">>, 5000},
    {<<"boss">>, <<"jim">>}
  ]},
  Jim = #item{uri= <<"jim">>, label= <<"Jim">>, types=[<<"manager">>], properties=[
    {<<"age">>, 40},
    {<<"salary">>, 10000},
    {<<"boss">>, <<"theboss">>},
    {<<"manages">>, [<<"paul">>]}
  ]},
  [Paul, Jim].

test_properties() ->
  Manages = #property{uri= <<"manages">>, label= <<"Manages">>, ranges=[<<"employee">>],
    arity=many, inverse= <<"boss">>},
  Boss = #property{uri= <<"boss">>, label= <<"Boss">>, ranges=[<<"manager">>],
    arity=many, inverse= <<"manages">>},
  Salary = #property{uri= <<"salary">>, label= <<"Salary">>, ranges= <<"number">>},
  [Manages, Boss, Salary].
