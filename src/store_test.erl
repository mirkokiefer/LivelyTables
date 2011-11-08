-module(store_test).

-export([test/0]).

-include("../include/records.hrl").

test() ->
  test_write_core(),
  test_write_types(),
  {ok, success}.

test_write_core() ->
  {ok, success} = store:write_all(core_types()),
  {ok, success} = store:write_all(core_properties()).

test_write_types() ->
  {ok, success} = store:write_all(test_types()).

core_types() ->
  Item = #type{uri= ?ITEM, label= <<"Item">>, parents=[], legal_properties=[
    ?PROPERTY_LABEL,
    ?PROPERTY_TYPES
  ]},
  Type = #type{uri= ?TYPE, label= <<"Type">>, legal_properties=[
    ?PROPERTY_LEGALPROPERTIES,
    ?PROPERTY_PARENTS
  ]},
  Property = #type{uri= ?PROPERTY, label= <<"Property">>,
    legal_properties=[
      ?PROPERTY_RANGE,
      ?PROPERTY_ARITY
    ]
  },
  [Item, Type, Property].

core_properties() ->
  Label = #property{uri= ?PROPERTY_LABEL, label= <<"Label">>, range=?PROPERTY_TYPE_STRING},
  Types = #property{uri= ?PROPERTY_TYPES, label= <<"Types">>, range=?TYPE, arity=?ARITY_MANY},
  Parents = #property{uri= ?PROPERTY_PARENTS, label= <<"Parents">>, range=?TYPE, arity=?ARITY_MANY},
  LegalProps = #property{uri= ?PROPERTY_LEGALPROPERTIES, label= <<"Legal Properties">>,
    range=?PROPERTY, arity=?ARITY_MANY},
  Range = #property{uri=?PROPERTY_RANGE, label= <<"Range">>, range=?TYPE, arity=?ARITY_ONE},
  Arity = #property{uri=?PROPERTY_ARITY, label= <<"Arity">>, range=?PROPERTY_TYPE_STRING},
  Inverse = #property{uri=?PROPERTY_INVERSE, label= <<"Inverse">>, range=?PROPERTY, optional=true},
  Optional = #property{uri=?PROPERTY_OPTIONAL, label= <<"Optional">>, range=?PROPERTY_TYPE_BOOLEAN},
  [Label, Types, Parents, LegalProps, Range, Arity, Inverse, Optional].

test_types() ->
  Person = #type{uri= <<"person">>, label= <<"Person">>, legal_properties=[<<"age">>]},
  Employee = #type{uri= <<"employee">>, label= <<"Employee">>, parents=[<<"person">>],
    legal_properties=[<<"salary">>, <<"boss">>]},
  Manager = #type{uri= <<"manager">>, label= <<"Manager">>, parents=[<<"employee">>],
    legal_properties=[<<"manages">>]},
  [Person, Employee, Manager].