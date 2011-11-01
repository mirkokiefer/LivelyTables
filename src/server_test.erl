-module(server_test).

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
  Person = #type{uri="type/person", label="Person", legal_properties=["property/age"]},
  Employee = #type{uri="type/employee", label="Employee", parents=["type/person"],
    legal_properties=["property/salary", "property/boss"]},
  Manager = #type{uri="type/manager", label="Manager", parents=["type/employee"], legal_properties=["property/manages"]},
  [Person, Employee, Manager].

test_items() ->
  Paul = #item{uri="paul", label="Paul", types=["/type/employee"], properties=[
    {"property/salary", 5000},
    {"property/boss", "jim"}
  ]},
  Jim = #item{uri="jim", label="Jim", types=["/type/employee"], properties=[
    {"property/salary", 10000},
    {"property/boss", "theboss"},
    {"property/manages", ["paul"]}
  ]},
  [Paul, Jim].

test_properties() ->
  Manages = #property{uri="property/manages", label="Manages", ranges=["type/employee"], arity=many, inverse="property/boss"},
  Boss = #property{uri="property/boss", label="Boss", ranges=["type/manager"], arity=many, inverse="property/manages"},
  Salary = #property{uri="property/salary", label="Salary", ranges="type/number"},
  [Manages, Boss, Salary].