-module(test_data).
-export([core_types/0, core_properties/0, set_properties/0, set_types/0, types/0, composite_items/0]).
-export([items/0, properties/0, items_updated/0, invalid_items/0, invalid_items_updated/0, composite_items2/0]).

-include("../include/records.hrl").


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

types() ->
  Person = #type{uri= <<"person">>, label= <<"Person">>, legal_properties=[<<"age">>]},
  Employee = #type{uri= <<"employee">>, label= <<"Employee">>, parents=[<<"person">>],
    legal_properties=[<<"salary">>, <<"boss">>]},
  Manager = #type{uri= <<"manager">>, label= <<"Manager">>, parents=[<<"employee">>],
    legal_properties=[<<"manages">>]},
  [Person, Employee, Manager].

composite_items() ->
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

items() ->
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

items_updated() ->
  UpdatedPaul = #item{uri= <<"paul">>, properties=[
    {<<"boss">>, <<"jim">>}
  ]},
  [UpdatedPaul].

properties() ->
  Manages = #property{uri= <<"manages">>, label= <<"Manages">>, range= <<"employee">>,
    arity=?ARITY_MANY, optional=true},
  Boss = #property{uri= <<"boss">>, label= <<"Boss">>, range= <<"manager">>, inverse= <<"manages">>, optional=true},
  Salary = #property{uri= <<"salary">>, label= <<"Salary">>, range=?PROPERTY_TYPE_NUMBER},
  Age = #property{uri= <<"age">>, label= <<"Age">>, range=?PROPERTY_TYPE_NUMBER},
  [Manages, Boss, Salary, Age].

invalid_items() ->
  Paul = #item{uri= <<"paul">>, label= <<"Paul">>, types=[<<"employee">>], properties=[
    {<<"age">>, <<"40">>},
    {<<"salary">>, 5000},
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

invalid_items_updated() ->
  Paul = #item{uri= <<"paul">>, properties=[
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

composite_items2() ->
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