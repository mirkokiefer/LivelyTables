-module(utils).
-export([encode/1, generic2item/1, item2generic/1, generic2type/1, type2generic/1,
  generic2property/1, property2generic/1]).

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

generic2item(#generic{uri=URI, properties=Properties}) ->
  generic2item(Properties, #item{uri=URI}).

generic2item([{PropertyURI, Value}|Rest], Item=#item{properties=Properties}) ->
  NewItem = case PropertyURI of
    ?LABEL_URI -> Item#item{label=Value};
    ?TYPES_URI -> Item#item{types=Value};
    _ -> Item#item{properties=[{PropertyURI, Value}|Properties]}
  end,
  generic2item(Rest, NewItem);
generic2item([], Item) -> Item.

item2generic(#item{uri=URI, label=Label, types=Types, properties=Properties}) ->
  #generic{uri=URI, properties=[
    {?LABEL_URI, Label},
    {?TYPES_URI, Types}
  ] ++ Properties}.

generic2type(#generic{uri=URI, properties=Properties}) ->
  generic2type(Properties, #type{uri=URI}).

generic2type([{PropertyURI, Value}|Rest], Type=#type{properties=Properties}) ->
  NewType = case PropertyURI of
    ?LABEL_URI -> Type#type{label=Value};
    ?TYPES_URI -> Type#type{types=Value};
    ?PARENTS_URI -> Type#type{parents=Value};
    ?LEGALPROPERTIES_URI -> Type#type{legal_properties=Value};
    _ -> Type#type{properties=[{PropertyURI, Value}|Properties]}
  end,
  generic2type(Rest, NewType);
generic2type([], Type) -> Type.

type2generic(Type) ->
  #generic{uri=Type#type.uri, properties=[
    {?LABEL_URI, Type#type.label},
    {?TYPES_URI, Type#type.types},
    {?PARENTS_URI, Type#type.parents},
    {?LEGALPROPERTIES_URI, Type#type.legal_properties}
  ] ++ Type#type.properties}.

generic2property(#generic{uri=URI, properties=Properties}) ->
  generic2property(Properties, #property{uri=URI}).

generic2property([{PropertyURI, Value}|Rest], Property=#property{properties=Properties}) ->
  NewProperty = case PropertyURI of
    ?LABEL_URI -> Property#property{label=Value};
    ?TYPES_URI -> Property#property{types=Value};
    ?RANGES_URI -> Property#property{ranges=Value};
    ?ARITY_URI -> Property#property{arity=Value};
    ?INVERSE_URI -> Property#property{inverse=Value};
    _ -> Property#property{properties=[{PropertyURI, Value}|Properties]}
  end,
  generic2property(Rest, NewProperty);
generic2property([], Property) -> Property.

property2generic(Property) ->
  #generic{uri=Property#property.uri, properties=[
    {?LABEL_URI, Property#property.label},
    {?TYPES_URI, Property#property.types},
    {?RANGES_URI, Property#property.ranges},
    {?ARITY_URI, Property#property.arity},
    {?INVERSE_URI, Property#property.inverse}
  ] ++ Property#property.properties}.