-module(utils).
-export([log/1, encode/1, set/1, item2type/1, type2item/1,
  item2property/1, property2item/1,
  json/1, json2item/1, json2type/1, json2property/1]).

-include("../include/records.hrl").

log(Message) -> io:format("~p~n", [Message]).

encode(String) ->
  unicode:characters_to_binary(io_lib:format("~ts", [String])).

set(List) ->
  sets:to_list(sets:from_list(List)).

item2type(#item{uri=URI, label=Label, properties=Properties}) ->
  item2type(Properties, #type{uri=URI, label=Label}).

item2type([{PropertyURI, Value}|Rest], Type=#type{properties=Properties}) ->
  NewType = case PropertyURI of
    ?PROPERTY_PARENTS -> Type#type{parents=Value};
    ?PROPERTY_LEGALPROPERTIES -> Type#type{legal_properties=Value};
    _ -> Type#type{properties=[{PropertyURI, Value}|Properties]}
  end,
  item2type(Rest, NewType);
item2type([], Type) -> Type.

type2item(Type) ->
  #item{uri=Type#type.uri, label=Type#type.label, types=Type#type.types, properties=[
    {?PROPERTY_PARENTS, Type#type.parents},
    {?PROPERTY_LEGALPROPERTIES, Type#type.legal_properties}
  ] ++ Type#type.properties}.

item2property(#item{uri=URI, label=Label, properties=Properties}) ->
  item2property(Properties, #property{uri=URI, label=Label}).

item2property([{PropertyURI, Value}|Rest], Property=#property{properties=Properties}) ->
  NewProperty = case PropertyURI of
    ?PROPERTY_RANGE -> Property#property{range=Value};
    ?PROPERTY_ARITY -> Property#property{arity=Value};
    ?PROPERTY_INVERSE -> Property#property{inverse=Value};
    ?PROPERTY_OPTIONAL -> Property#property{optional=Value};
    _ -> Property#property{properties=[{PropertyURI, Value}|Properties]}
  end,
  item2property(Rest, NewProperty);
item2property([], Property) -> Property.

property2item(Property) ->
  Item=#item{uri=Property#property.uri, label=Property#property.label, types=Property#property.types,
    properties=[
      {?PROPERTY_RANGE, Property#property.range},
      {?PROPERTY_ARITY, Property#property.arity},
      {?PROPERTY_OPTIONAL, Property#property.optional}
    ] ++ Property#property.properties
  },
  #item{properties=Properties}=Item,
  case Property#property.inverse of
    undefined -> Item;
    Value -> Item#item{properties=[{?PROPERTY_INVERSE, Value}|Properties]}
  end.

json([]) -> mochijson2:encode([]);

json([First|Rest]) ->
  mochijson2:encode(json([First|Rest], []));

json(Element) -> mochijson2:encode(struct(Element)).

json([First|Rest], Structs) ->
  json(Rest, [struct(First)|Structs]);
json([], Structs) -> Structs.

struct(Item=#item{uri=URI}) ->
  Properties = case URI of
    undefined -> [];
    _ -> [{?URI, URI}]
  end,
  {struct, Properties++[
    {?PROPERTY_LABEL, Item#item.label},
    {?PROPERTY_TYPES, Item#item.types}
  ] ++ resolve_properties(Item#item.properties)};

struct(Type=#type{}) ->
  {struct, Properties} = struct(type2item(Type)),
  {struct, Properties ++ [
    {?PROPERTY_PARENTS, Type#type.parents},
    {?PROPERTY_LEGALPROPERTIES, Type#type.legal_properties}
  ]};

struct(Property=#property{}) ->
  Properties =[
    {?PROPERTY_RANGE, Property#property.range},
    {?PROPERTY_ARITY, Property#property.arity},
    {?PROPERTY_OPTIONAL, Property#property.optional}
  ] ++ Property#property.properties,
  NewProperties = case Property#property.inverse of
    undefined -> Properties;
    Value -> Properties ++ [{?PROPERTY_INVERSE, Value}]
  end,
  {struct, ItemProperties} = struct(property2item(Property)),
  {struct, ItemProperties ++ NewProperties};

struct(Any) -> Any.

resolve(Item=#item{}) -> struct(Item);
resolve(URI) -> URI.

resolve_properties([{Property, Value}|Rest]) ->
  [{Property, resolve(Value)}|resolve_properties(Rest)];
resolve_properties([]) -> [].

json2item({struct, Elements}) ->
  parse_item_elements(Elements, #item{}).

json2type({struct, Elements}) ->
  item2type(parse_item_elements(Elements, #item{})).

json2property({struct, Elements}) ->
  item2property(parse_item_elements(Elements, #item{})).

parse_item_elements([{Key, Value}|Rest], Item=#item{properties=Properties}) ->
  NewItem = case Key of
    <<"uri">> -> Item#item{uri=Value};
    <<"label">> -> Item#item{label=Value};
    <<"types">> -> Item#item{types=Value};
    _ -> Item#item{properties=[{Key, parse_property_value(Value)}|Properties]}
  end,
  parse_item_elements(Rest, NewItem);
parse_item_elements([], Item) -> Item.

parse_property_value([]) -> [];
parse_property_value([First|Rest]) -> [parse_property_value(First)|parse_property_value(Rest)];
parse_property_value({struct, Elements}) -> json2item({struct, Elements});
parse_property_value(Literal) -> Literal.
