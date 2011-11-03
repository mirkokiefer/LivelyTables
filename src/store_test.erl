-module(store_test).

-export([test/0, bad_item/0]).

-include("../include/records.hrl").

test() ->
  test_item_write(),
  test_type_write(),
  test_property_write(),
  test_item_read(),
  test_type_read(),
  test_property_read(),
  test_write_core(),
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

test_write_core() ->
  {ok, success} = store:write_all(core_types()),
  {ok, success} = store:write_all(core_properties()).

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
    {<<"boss">>, <<"jim">>},
    {<<"manages">>, [<<"paul">>]}
  ]},
  [Paul, Jim].

test_properties() ->
  Manages = #property{uri= <<"manages">>, label= <<"Manages">>, ranges=[<<"employee">>],
    arity=?ARITY_MANY, inverse= <<"boss">>},
  Boss = #property{uri= <<"boss">>, label= <<"Boss">>, ranges=[<<"manager">>], inverse= <<"manages">>},
  Salary = #property{uri= <<"salary">>, label= <<"Salary">>, ranges= [?PROPERTY_TYPE_NUMBER]},
  Age = #property{uri= <<"age">>, label= <<"Age">>, ranges= [?PROPERTY_TYPE_NUMBER]},
  [Manages, Boss, Salary, Age].

core_types() ->
  Item = #type{uri= ?ITEM, label= <<"Item">>, parents=[], legal_properties=[
    ?PROPERTY_LABEL,
    ?PROPERTY_TYPES
  ]},
  Type = #type{uri= ?TYPE, label= <<"Type">>, parents= [?ITEM], legal_properties=[
    ?PROPERTY_LEGALPROPERTIES,
    ?PROPERTY_PARENTS
  ]},
  Property = #type{uri= ?PROPERTY, label= <<"Property">>, types=[?TYPE], parents= [?ITEM],
    legal_properties=[
      ?PROPERTY_RANGES,
      ?PROPERTY_ARITY
    ]
  },
  [Item, Type, Property].

core_properties() ->
  Label = #property{uri= ?PROPERTY_LABEL, label= <<"Label">>, ranges=[?PROPERTY_TYPE_STRING]},
  Types = #property{uri= ?PROPERTY_TYPES, label= <<"Types">>, ranges=[?TYPE], arity=?ARITY_MANY},
  Parents = #property{uri= ?PROPERTY_PARENTS, label= <<"Parents">>, ranges=[?TYPE], arity=?ARITY_MANY},
  Ranges = #property{uri=?PROPERTY_RANGES, label= <<"Range">>, ranges=[?TYPE], arity=?ARITY_MANY},
  Arity = #property{uri=?PROPERTY_ARITY, label= <<"Arity">>, ranges=[?PROPERTY_TYPE_STRING]},
  Inverse = #property{uri=?PROPERTY_INVERSE, label= <<"Inverse">>, ranges=[?PROPERTY]},
  [Label, Types, Parents, Ranges, Arity, Inverse].

bad_item() ->
  #item{uri= <<"paul">>, label= <<"Paul">>, types=[<<"employee">>], properties=[
    {<<"age">>, <<"40">>},
    {<<"salary">>, 5000},
    {<<"boss">>, <<"jim">>}
  ]}.