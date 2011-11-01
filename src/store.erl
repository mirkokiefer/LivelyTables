
-module(store).
-export([init/0, reset/0, start/0, stop/0]).
-export([lookup/2]).
-export([test_store_items/0]).

-include_lib("stdlib/include/qlc.hrl" ).

-define(DB_PATH, "../output/tuples.tab").

-record(item, {uri, label, types=["type/thing"], properties=[]}).
-record(type, {uri, label, types=["type/type"], properties=[], parents=["type/thing"], legal_properties=[]}).
-record(property, {uri, label, types=["type/property"], ranges=["type/thing"], arity=one, inverse}).

tables() -> [item, type, property].

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(tables()),
  mnesia:stop().

create_tables([]) -> {ok, success};

create_tables([First|Rest]) ->
  mnesia:create_table(First, [
	{attributes, record_fields(First)},
    {disc_copies,[node()]}
  ]),
  create_tables(Rest).

reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]).

start() ->
  mnesia:start(),
  mnesia:wait_for_tables(tables(), 20000).

stop() ->
  mnesia:stop().

lookup(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Item} = mnesia:transaction(F),
  Item.

query_item(URI) ->
  do(qlc:q([Item || Item=#item{uri=U} <- mnesia:table(item), U==URI])).

test_store_items() ->
  store_items(test_types() ++ test_items()).

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
    {"property/manages", ["jim"]}
  ]},
  [Paul, Jim].

store_items(Records) ->
  F = fun() -> store_items_transaction(Records) end,
  mnesia:transaction(F).

store_items_transaction([]) -> {ok, success};

store_items_transaction([First|Rest]) ->
  mnesia:write(First),
  store_items_transaction(Rest).

store(Record) ->
  F = fun() -> mnesia:write(Record) end,
  mnesia:transaction(F).

% utility functions
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

record_fields(Record) ->
  case Record of
    item -> record_info(fields, item);
    type -> record_info(fields, type);
    property -> record_info(fields, property)
  end.