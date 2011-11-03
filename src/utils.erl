-module(utils).
-export([encode/1, item2type/1, type2item/1,
  item2property/1, property2item/1]).

-include("../include/records.hrl").

-define(LABEL_URI, <<"label">>).
-define(TYPES_URI, <<"types">>).
-define(PARENTS_URI, <<"parents">>).
-define(LEGALPROPERTIES_URI, <<"legal_properties">>).
-define(RANGES_URI, <<"ranges">>).
-define(ARITY_URI, <<"arity">>).
-define(INVERSE_URI, <<"inverse">>).


encode(String) ->
  unicode:characters_to_binary(io_lib:format("~ts", [String])).

item2type(#item{uri=URI, label=Label, types=Types, properties=Properties}) ->
  item2type(Properties, #type{uri=URI, label=Label, types=Types}).

item2type([{PropertyURI, Value}|Rest], Type=#type{properties=Properties}) ->
  NewType = case PropertyURI of
    ?PARENTS_URI -> Type#type{parents=Value};
    ?LEGALPROPERTIES_URI -> Type#type{legal_properties=Value};
    _ -> Type#type{properties=[{PropertyURI, Value}|Properties]}
  end,
  item2type(Rest, NewType);
item2type([], Type) -> Type.

type2item(Type) ->
  #item{uri=Type#type.uri, label=Type#type.label, types=Type#type.types, properties=[
    {?PARENTS_URI, Type#type.parents},
    {?LEGALPROPERTIES_URI, Type#type.legal_properties}
  ] ++ Type#type.properties}.

item2property(#item{uri=URI, label=Label, types=Types, properties=Properties}) ->
  item2property(Properties, #property{uri=URI, label=Label, types=Types}).

item2property([{PropertyURI, Value}|Rest], Property=#property{properties=Properties}) ->
  NewProperty = case PropertyURI of
    ?RANGES_URI -> Property#property{ranges=Value};
    ?ARITY_URI -> Property#property{arity=Value};
    ?INVERSE_URI -> Property#property{inverse=Value};
    _ -> Property#property{properties=[{PropertyURI, Value}|Properties]}
  end,
  item2property(Rest, NewProperty);
item2property([], Property) -> Property.

property2item(Property) ->
  #item{uri=Property#property.uri, label=Property#property.label, types=Property#property.types,
    properties=[
      {?RANGES_URI, Property#property.ranges},
      {?ARITY_URI, Property#property.arity},
      {?INVERSE_URI, Property#property.inverse}
    ] ++ Property#property.properties
  }.