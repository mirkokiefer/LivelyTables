
-module(store).
-export([init/0, reset/0, start/0, stop/0]).
-export([write_all/1, read_item/1, read_type/1, read_property/1, read_items_of_type/1,
  read_parents/1, read_subtypes/1]).

-include("../include/records.hrl").

-include_lib("stdlib/include/qlc.hrl" ).

-define(DB_PATH, "../output/tuples.tab").

-record(item_table, {uri, label, properties=[]}).
-record(item_type_table, {item, type}).
-record(type_table, {uri, legal_properties=[]}).
-record(type_parent_table, {type, parent}).
-record(property_table, {uri, ranges, arity=one, inverse}).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(),
  mnesia:stop().

create_tables() ->
  mnesia:create_table(item_table, [
    {attributes, record_info(fields, item_table)},
    {type, set},
    {disc_copies,[node()]}
  ]),
  mnesia:create_table(item_type_table, [
    {attributes, record_info(fields, item_type_table)},
    {type, bag},
    {disc_copies,[node()]},
    {index, [type]}
  ]),
  mnesia:create_table(type_table, [
    {attributes, record_info(fields, type_table)},
    {type, set},
    {disc_copies,[node()]}
  ]),
  mnesia:create_table(type_parent_table, [
    {attributes, record_info(fields, type_parent_table)},
    {type, bag},
    {disc_copies,[node()]},
    {index, [parent]}
  ]),
  mnesia:create_table(property_table, [
    {attributes, record_info(fields, property_table)},
    {type, set},
    {disc_copies,[node()]}
  ]).

reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  init(),
  start().

start() ->
  mnesia:start(),
  Tables = [item_table, item_type_table, type_table, type_parent_table, property_table],
  mnesia:wait_for_tables(Tables, 20000).

stop() ->
  mnesia:stop().

write_all(Records) ->
  F = fun() -> write_each(Records) end,
  mnesia:transaction(F),
  {ok, success}.

write_each([First|Rest]) ->
  write(First),
  write_each(Rest);
write_each([]) -> {ok, success}.

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

store_table_records([First|Rest]) ->
  mnesia:write(First),
  store_table_records(Rest);
store_table_records([]) -> {ok, success}.

read_item(URI) ->
  case read(item_table, URI) of
    [#item_table{uri=URI, label=Label, properties=Props}] ->
      Types = [Type || #item_type_table{type=Type} <- read(item_type_table, URI)],
      #item{uri=URI, label=Label, types=Types, properties=Props};
    [] -> undefined
  end.

read_type(URI) ->
  case read_item(URI) of
    #item{uri=URI, label=Label, types=Types, properties=Props} ->
      [#type_table{legal_properties=LegalProps}] = read(type_table, URI),
      Parents = [Parent || #type_parent_table{parent=Parent} <- read(type_parent_table, URI)],
      #type{uri=URI, label=Label, types=Types, properties=Props, parents=Parents, legal_properties=LegalProps};
    undefined -> undefined
  end.

read_property(URI) ->
  case read_item(URI) of
    #item{uri=URI, label=Label, types=Types, properties=Props} ->
      [#property_table{ranges=Ranges, arity=Arity, inverse=Inverse}] = read(property_table, URI),
      #property{uri=URI, label=Label, types=Types, properties=Props, ranges=Ranges, arity=Arity, inverse=Inverse};
    undefined -> undefined
  end.

read_items_of_type(TypeURI) ->
  F = fun() -> mnesia:index_read(item_type_table, TypeURI, #item_type_table.type) end,
  {atomic, Records} = mnesia:transaction(F),
  Items = [Item || #item_type_table{item=Item} <- Records],
  Items ++ lists:flatten([read_items_of_type(Subtype) || Subtype <- read_subtypes(TypeURI)]).

read_parents(TypeURI) ->
  Records = read(type_parent_table, TypeURI),
  [Parent || #type_parent_table{parent=Parent} <- Records].

read_subtypes(TypeURI) ->
  F = fun() -> mnesia:index_read(type_parent_table, TypeURI, #type_parent_table.parent) end,
  {atomic, Records} = mnesia:transaction(F),
  [Subtype || #type_parent_table{type=Subtype} <- Records].

read(Table, Key) ->
  F = fun() -> mnesia:read(Table, Key) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

query_item(URI) ->
  do(qlc:q([Item || Item=#item{uri=U} <- mnesia:table(item), U==URI])).

% utility functions
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
