-module(test_data).
-export([core_types/0, core_properties/0, set_properties/0, set_types/0, types/0, composite_items/0]).
-export([items/0, properties/0, items_updated/0, invalid_items/0, invalid_items_updated/0, composite_items2/0]).
-export([sample_set/0]).

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

  TypeList = #property{uri= ?PROPERTY_TYPE_LIST, label= <<"Type list">>, range=?TYPE, arity=?ARITY_MANY},
  ItemList = #property{uri= ?PROPERTY_ITEM_LIST, label= <<"Item list">>, range=?ITEM, arity=?ARITY_MANY},
  PropertyList = #property{uri= ?PROPERTY_PROPERTY_LIST, label= <<"Property list">>, range=?PROPERTY,
    arity=?ARITY_MANY},

  Set = #property{uri= ?PROPERTY_SET, label= <<"Set">>, range=?PROPERTY},
  TypeSet = #property{uri= ?PROPERTY_TYPE_SET, label= <<"Type set">>, range=?SET},
  ItemSet = #property{uri= ?PROPERTY_ITEM_SET, label= <<"Item set">>, range=?SET},
  PropertySet = #property{uri= ?PROPERTY_PROPERTY_SET, label= <<"Property list">>, range=?SET},

  ValueCondition = #property{uri= ?PROPERTY_VALUE_CONDITION, label= <<"Value Condition">>, range=?VALUE_CONDITION},
  Value = #property{uri= ?PROPERTY_VALUE, label= <<"Value">>, range=?ITEM},

  [Sets, TypeList, ItemList, PropertyList, Set, TypeSet, ItemSet, PropertySet, ValueCondition, Value].


set_types() ->
  Set = #type{uri= ?SET, label= <<"Set">>, legal_properties=[]},
  [Set | set_operations() ++ set_transforms() ++ set_filters()].

set_operations() ->
  SetOperation = #type{uri= ?SET_OPERATION, label= <<"Set Operation">>, parents=[?SET],
    legal_properties=[?PROPERTY_SETS]},
  Union = #type{uri= ?UNION, label= <<"Union">>, parents=[?SET_OPERATION], legal_properties=[]},
  Intersection = #type{uri= ?INTERSECTION, label= <<"Intersection">>, parents=[?SET_OPERATION], legal_properties=[]},
  [SetOperation, Union, Intersection].

set_transforms() ->
  SetTransform = #type{uri= ?TRANSFORM_SET, label= <<"Set Transform">>,
    parents=[?SET], legal_properties=[?PROPERTY_SET]},
  ValuesToItems = #type{uri= ?TRANSFORM_VALUES_TO_ITEMS, label= <<"Property Values -> Items">>,
    parents=[?TRANSFORM_SET], legal_properties=[?PROPERTY_PROPERTY_LIST]},
  PropertiesToPropertyItems = #type{uri= ?TRANSFORM_PROPERTIES_TO_ITEMS, label= <<"Properties -> Property Items">>,
    parents=[?TRANSFORM_SET], legal_properties=[]},
  PropertyItemsToItems = #type{uri= ?TRANSFORM_PROPERTY_ITEMS_TO_ITEMS, label= <<"Property Items -> Items">>,
    parents=[?TRANSFORM_SET], legal_properties=[]},
  [SetTransform, ValuesToItems, PropertiesToPropertyItems, PropertyItemsToItems].

set_filters() ->
  Filter = #type{uri= ?FILTER, label= <<"Filter">>, parents=[?SET], legal_properties=[?PROPERTY_SET]},

  TypeFilter = #type{uri= ?FILTER_TYPES, label= <<"Type Filter">>, parents=[?FILTER]},
  TypeFilterList = #type{uri= ?FILTER_TYPES_LIST, label= <<"Type Filter List">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_TYPE_LIST]},
  TypeFilterSet = #type{uri= ?FILTER_TYPES_SET, label= <<"Type Filter Set">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_TYPE_SET]},

  ItemFilter = #type{uri= ?FILTER_ITEMS, label= <<"Item Filter">>,
    parents=[?FILTER], legal_properties=[]},
  ItemFilterList = #type{uri= ?FILTER_ITEMS, label= <<"Item Filter List">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_ITEM_LIST]},
  ItemFilterSet = #type{uri= ?FILTER_ITEMS, label= <<"Item Filter Set">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_ITEM_SET]},

  PropertyExistenceFilter = #type{uri= ?FILTER_PROPERTY_EXISTENCE, label= <<"Property Existence Filter">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_PROPERTY_LIST]},
  PropertyExistenceFilterList = #type{uri= ?FILTER_PROPERTY_EXISTENCE_LIST,
    label= <<"Property Existence Filter List">>, parents=[?FILTER], legal_properties=[?PROPERTY_PROPERTY_LIST]},
  PropertyExistenceFilterSet = #type{uri= ?FILTER_PROPERTY_EXISTENCE_SET,
    label= <<"Property Existence Filter Set">>, parents=[?FILTER], legal_properties=[?PROPERTY_PROPERTY_SET]},

  PropertyValueFilter = #type{uri= ?FILTER_PROPERTY_VALUE, label= <<"Property Value Filter">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_VALUE_CONDITION]},
  PropertyValueFilterList = #type{uri= ?FILTER_PROPERTY_VALUE_LIST, label= <<"Property Value Filter List">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_PROPERTY_LIST]},
  PropertyValueFilterSet = #type{uri= ?FILTER_PROPERTY_VALUE_SET, label= <<"Property Value Filter Set">>,
    parents=[?FILTER], legal_properties=[?PROPERTY_PROPERTY_SET]},

  ValueCondition = #type{uri= ?VALUE_CONDITION, label= <<"Value Condition">>, legal_properties=[]},
  ConditionEqual = #type{uri= ?CONDITION_EQUAL, label= <<"Condition Equal">>,
    parents=[?VALUE_CONDITION], legal_properties=[?PROPERTY_VALUE]},

  [Filter, TypeFilter, TypeFilterList, TypeFilterSet,
    ItemFilter, ItemFilterList, ItemFilterSet,
    PropertyExistenceFilter, PropertyExistenceFilterList, PropertyExistenceFilterSet,
    PropertyValueFilter, PropertyValueFilterList, PropertyValueFilterSet,
    ValueCondition, ConditionEqual
  ].

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

sample_set() ->
  Persons = #item{types=[?FILTER_TYPES], properties=[
    {?PROPERTY_TYPE_LIST, [<<"person">>]}
  ]},
  ValueCondition = #item{types=[?FILTER_PROPERTY_VALUE], properties=[
    {?PROPERTY_PROPERTY_LIST, [<<"boss">>]},
    {?PROPERTY_VALUE_CONDITION, #item{types=[?CONDITION_EQUAL], properties=[
      {?PROPERTY_VALUE, <<"jim">>}
    ]}}
  ]},
  Set = #item{uri= <<"sample_set">>, label= <<"Persons with Boss Jim">>, types=[?INTERSECTION], properties=[
    {?PROPERTY_SETS, [Persons, ValueCondition]}
  ]},
  Set.