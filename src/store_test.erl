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
  {ok, success} = store:write_all(core_types()),
  {ok, success} = store:write_all(core_properties()).

test_write_types() ->
  {ok, success} = store:write_all(test_types()).

test_write_composite() ->
  {ok, success} = store:write_all(test_composite()).

test_write_set_core() ->
  {ok, success} = store:write_all(set_properties()),
  {ok, success} = store:write_all(set_types()).

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

set_properties() ->
  Sets = #property{uri= ?PROPERTY_SETS, label= <<"Sets">>, range=?SET, arity=?ARITY_MANY},
  OnProperty = #property{uri= ?PROPERTY_ON_PROPERTY, label= <<"On Property">>, range=?PROPERTY},
  ValueCondition = #property{uri= ?PROPERTY_VALUE_CONDITION, label= <<"Value Condition">>, range=?VALUE_CONDITION},
  Value = #property{uri= ?PROPERTY_VALUE, label= <<"Value">>, range=?ITEM},
  [Sets, OnProperty, ValueCondition, Value].


set_types() ->
  Set = #type{uri= ?SET, label= <<"Set">>, legal_properties=[]},
  SetOperation = #type{uri= ?SET_OPERATION, label= <<"Set Operation">>, parents=[?SET],
    legal_properties=[?PROPERTY_SETS]},
  Union = #type{uri= ?UNION, label= <<"Union">>, parents=[?SET_OPERATION], legal_properties=[]},
  Intersection = #type{uri= ?INTERSECTION, label= <<"Intersection">>, parents=[?SET_OPERATION], legal_properties=[]},
  Filter = #type{uri= ?FILTER, label= <<"Filter">>, parents=[?SET], legal_properties=[]},
  PropertyExistenceFilter = #type{uri= ?PROPERTY_EXISTENCE_FILTER, label= <<"Property Existence Filter">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_ON_PROPERTY]},
  PropertyValueFilter = #type{uri= ?PROPERTY_VALUE_FILTER, label= <<"Property Value Filter">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_ON_PROPERTY, ?PROPERTY_VALUE_CONDITION]},
  ValueCondition = #type{uri= ?VALUE_CONDITION, label= <<"Value Condition">>, legal_properties=[]},
  ValueConditionExact = #type{uri= ?VALUE_CONDITION_EXACT, label= <<"Value Condition Exact">>, parents=[?SET],
    legal_properties=[?PROPERTY_VALUE]},
  [Set, SetOperation, Union, Intersection, Filter, PropertyExistenceFilter, PropertyValueFilter,
    ValueCondition, ValueConditionExact].

test_types() ->
  Person = #type{uri= <<"person">>, label= <<"Person">>, legal_properties=[<<"age">>]},
  Employee = #type{uri= <<"employee">>, label= <<"Employee">>, parents=[<<"person">>],
    legal_properties=[<<"salary">>, <<"boss">>]},
  Manager = #type{uri= <<"manager">>, label= <<"Manager">>, parents=[<<"employee">>],
    legal_properties=[<<"manages">>]},
  [Person, Employee, Manager].

test_composite() ->
  RealEstate = #type{uri= <<"real_estate">>, label= <<"Real Estate">>, legal_properties=[
    #property{uri= <<"accomodates">>, label= <<"accomodates">>, range= <<"person">>, arity= <<"many">>}
  ]},
  Company = #type{uri= <<"company">>, label= <<"Company">>, legal_properties=[
    #property{uri= <<"company_owns">>, label= <<"owns">>, range= RealEstate, optional=true}
  ]},
  SomeCompany = #item{uri= <<"some_company">>, label= <<"Some Company">>, types=[Company], properties=[
    {<<"company_owns">>, #item{label= <<"A House">>, types=[<<"real_estate">>], properties=[
      {<<"accomodates">>, #item{uri= <<"bob">>, label= <<"Bob">>, types=[<<"person">>], properties=[{<<"age">>, 20}]}}
    ]}}
  ]},
  [SomeCompany].