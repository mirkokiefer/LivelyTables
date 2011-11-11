
-module(store).
-export([init/0, reset/0, start/0, stop/0, clear/0]).
-export([transaction/1, write_all/1, read_item/1, read_type/1, read_property/1,
  read_type_item/1, read_property_item/1,
  read_items_of_type/1, read_types_of_item/1, read_direct_types_of_item/1,
  read_parents/1, read_direct_parents/1,
  read_subtypes/1, read_direct_subtypes/1]).

-include("../include/records.hrl").

-include_lib("stdlib/include/qlc.hrl" ).

-define(DB_PATH, "../output/tuples.tab").

-define(TABLES, [item_table, item_type_table, type_table, type_parent_table, property_table]).
-record(item_table, {uri, label, properties=[]}).
-record(item_type_table, {item, type}).
-record(type_table, {uri, legal_properties=[]}).
-record(type_parent_table, {type, parent}).
-record(property_table, {uri, range, arity=one, inverse, optional}).

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

% deletes and re-creates the schema
reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  init(),
  start().

% deletes all table entries
clear() ->
  [mnesia:clear_table(Tab) || Tab <- ?TABLES].

start() ->
  mnesia:start(),
  Tables = [item_table, item_type_table, type_table, type_parent_table, property_table],
  mnesia:wait_for_tables(Tables, 20000).

stop() ->
  mnesia:stop().

transaction(Fun) ->
  mnesia:transaction(Fun).

write_all(Records) ->
  [write(Record) || Record <- Records],
  {ok, success}.

write(#item{uri=URI, label=Label, types=Types, properties=Properties}) ->
  ItemTableRecord = #item_table{uri=URI, label=Label, properties=resolve_properties(Properties)},
  ItemTypeTableRecords = [#item_type_table{item=URI, type=resolve(Type)} || Type <- Types],
  store_table_records([ItemTableRecord|ItemTypeTableRecords]);

write(#type{uri=URI, label=Label, types=Types, properties=Props, parents=Parents, legal_properties=LegalProps}) ->
  write(#item{uri=URI, label=Label, types=Types, properties=Props}),
  TypeTableRecord = #type_table{uri=URI, legal_properties=resolve(LegalProps)},
  TypeParentTableRecords = [#type_parent_table{type=URI, parent=resolve(Parent)} || Parent <- Parents],
  store_table_records([TypeTableRecord|TypeParentTableRecords]);

write(#property{uri=URI, label=Label, types=Types, properties=Props, range=Range,
  arity=Arity, inverse=Inverse, optional=Optional}) ->
    write(#item{uri=URI, label=Label, types=Types, properties=Props}),
    store_table_records([#property_table{uri=URI, range=resolve(Range), arity=Arity,
      inverse=resolve(Inverse), optional=Optional}
    ]).

% resolve embedded items to their URIs if they exist and store them separately
resolve([]) -> [];

resolve([First|Rest]) ->
  [resolve(First)|resolve(Rest)];

resolve(Item=#item{uri=undefined, types=Types, properties=Properties}) ->
  Item#item{types=resolve(Types), properties=resolve_properties(Properties)};

resolve(Item=#item{uri=URI}) ->
  write(Item),
  URI;

resolve(Property=#property{uri=URI}) ->
  write(Property),
  URI;

resolve(Type=#type{uri=URI}) ->
  write(Type),
  URI;

resolve(URI) -> URI.

resolve_properties([{Property, Value}|Rest]) ->
  [{Property, resolve(Value)}|resolve_properties(Rest)];
resolve_properties([]) -> [].

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
      [#property_table{range=Range, arity=Arity, inverse=Inverse, optional=Optional}] = read(property_table, URI),
      #property{uri=URI, label=Label, types=Types, properties=Props, range=Range,
        arity=Arity, inverse=Inverse, optional=Optional};
    undefined -> undefined
  end.

read_type_item(URI) ->
  case read_type(URI) of
    undefined -> undefined;
    Type -> utils:type2item(Type)
  end.

read_property_item(URI) ->
  case read_property(URI) of
    undefined -> undefined;
    Property -> utils:property2item(Property)
  end.

read_direct_types_of_item(ItemURI) ->
  [Type || #item_type_table{type=Type} <- read(item_type_table, ItemURI)].

read_types_of_item(ItemURI) -> utils:set(read_types_of_item_internal(ItemURI)).

read_types_of_item_internal(ItemURI) ->
  Types = read_direct_types_of_item(ItemURI),
  Types ++ lists:flatten([read_parents(Type) || Type <- Types]).

read_items_of_type(TypeURI) ->
  lists:flatten([read_direct_items_of_type(Each) || Each <- [TypeURI|read_subtypes(TypeURI)]]).

read_direct_items_of_type(TypeURI) ->
  F = fun() -> mnesia:index_read(item_type_table, TypeURI, #item_type_table.type) end,
  {atomic, Records} = mnesia:transaction(F),
  [Item || #item_type_table{item=Item} <- Records].

read_parents(TypeURI) ->
  DirectParents = read_direct_parents(TypeURI),
  DirectParents ++ lists:flatten([read_parents(Parent) || Parent <- DirectParents]).

read_direct_parents(TypeURI) ->
  [Parent || #type_parent_table{parent=Parent} <- read(type_parent_table, TypeURI)].

read_subtypes(TypeURI) ->
  DirectSubtypes = read_direct_subtypes(TypeURI),
  DirectSubtypes ++ lists:flatten([read_subtypes(Each) || Each <- DirectSubtypes]).

read_direct_subtypes(TypeURI) ->
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
