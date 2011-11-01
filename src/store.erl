
-module(store).
-export([init/0, reset/0, start/0, stop/0]).
-export([write_all/1, lookup/2, read_item/1, read_type/1, read_property/1]).

-include("../include/records.hrl").

-include_lib("stdlib/include/qlc.hrl" ).

-define(DB_PATH, "../output/tuples.tab").

-record(item_table, {uri, label, properties=[]}).
-record(item_type_table, {item, type}).
-record(type_table, {uri, legal_properties=[]}).
-record(type_parent_table, {type, parent}).
-record(property_table, {uri, ranges, arity=one, inverse}).

tables() -> [item_table, item_type_table, type_table, type_parent_table, property_table].

table_type(Table) ->
  case Table of
    item_table -> set;
    item_type_table -> bag;
    type_table -> set;
    type_parent_table -> bag;
    property_table -> set
  end.

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(tables()),
  mnesia:stop().

create_tables([]) -> {ok, success};

create_tables([First|Rest]) ->
  mnesia:create_table(First, [
	{attributes, record_fields(First)},
    {disc_copies,[node()]},
    {type, table_type(First)}
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

write_all(Records) ->
  F = fun() -> write_each(Records) end,
  mnesia:transaction(F),
  {ok, success}.

write_each([]) -> {ok, success};

write_each([First|Rest]) ->
  write(First),
  write_each(Rest).

write(#item{uri=URI, label=Label, types=Types, properties=Properties}) ->
  ItemTableRecord = #item_table{uri=URI, label=Label, properties=Properties},
  ItemTypeTableRecords = [#item_type_table{item=URI, type=Type} || Type <- Types],
  store_table_records([ItemTableRecord|ItemTypeTableRecords]);

write(#type{uri=URI, label=Label, types=Types, properties=Props, parents=Parents, legal_properties=LegalProps}) ->
  write(#item{uri=URI, label=Label, types=Types, properties=Props}),
  TypeTableRecord = #type_table{uri=URI, legal_properties=LegalProps},
  TypeParentTableRecords = [#type_parent_table{type=URI, parent=Parent} || Parent <- Parents],
  store_table_records([TypeTableRecord|TypeParentTableRecords]);

write(#property{uri=URI, label=Label, types=Types, properties=Props, ranges=Ranges, arity=Arity, inverse=Inverse}) ->
  write(#item{uri=URI, label=Label, types=Types, properties=Props}),
  store_table_records([#property_table{uri=URI, ranges=Ranges, arity=Arity, inverse=Inverse}]).

store_table_records([]) -> {ok, success};

store_table_records([First|Rest]) ->
  mnesia:write(First),
  store_table_records(Rest).

read_item(URI) ->
  case lookup(item_table, URI) of
    [#item_table{uri=URI, label=Label, properties=Props}] ->
      Types = [Type || #item_type_table{type=Type} <- lookup(item_type_table, URI)],
      #item{uri=URI, label=Label, types=Types, properties=Props};
    [] -> undefined
  end.

read_type(URI) ->
  case read_item(URI) of
    #item{uri=URI, label=Label, types=Types, properties=Props} ->
      [#type_table{legal_properties=LegalProps}] = lookup(type_table, URI),
      Parents = [Parent || #type_parent_table{parent=Parent} <- lookup(type_parent_table, URI)],
      #type{uri=URI, label=Label, types=Types, properties=Props, parents=Parents, legal_properties=LegalProps};
    undefined -> undefined
  end.

read_property(URI) ->
  case read_item(URI) of
    #item{uri=URI, label=Label, types=Types, properties=Props} ->
      [#property_table{ranges=Ranges, arity=Arity, inverse=Inverse}] = lookup(property_table, URI),
      #property{uri=URI, label=Label, types=Types, properties=Props, ranges=Ranges, arity=Arity, inverse=Inverse};
    undefined -> undefined
  end.

lookup(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Item} = mnesia:transaction(F),
  Item.

query_item(URI) ->
  do(qlc:q([Item || Item=#item{uri=U} <- mnesia:table(item), U==URI])).

% utility functions
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

record_fields(Record) ->
  case Record of
    item_table -> record_info(fields, item_table);
    item_type_table -> record_info(fields, item_type_table);
    type_table -> record_info(fields, type_table);
    type_parent_table -> record_info(fields, type_parent_table);
    property_table -> record_info(fields, property_table)
  end.