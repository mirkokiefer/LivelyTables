-module(server).

-export([start/0, stop/0, loop/1]).

-include("../include/records.hrl").

-define(HTTP_OPTS, [
            {loop, {?MODULE, loop}},
            {port, 8080},
            {name, http_sapiento}
            ]).

start() ->
  {ok, Http} = mochiweb_http:start(?HTTP_OPTS),
  Http.

stop() ->
  mochiweb_http:stop(http_routing).

loop(Req) ->
  Path = Req:get(path),
  respond(string:tokens(Path, "/"), lists:sort(Req:parse_qs()), Req).

respond(["type", TypeID], _, Req) ->
  Type = store:read_type(utils:encode(TypeID)),
  Json = type2json(Type),
  Req:ok({"text/plain;charset=utf-8", Json});

respond([TypeID], _, Req) ->
  ItemURIs = store:read_items_of_type(utils:encode(TypeID)),
  Json = list2json(ItemURIs),
  Req:ok({"text/plain;charset=utf-8", Json});

respond([TypeID, "_full"], _, Req) ->
  ItemURIs = store:read_items_of_type(utils:encode(TypeID)),
  Items = [store:read_item(ItemURI) || ItemURI <- ItemURIs],
  Json = itemlist2json(Items),
  Req:ok({"text/plain;charset=utf-8", Json});

respond(["property", URI], _, Req) ->
  Property = store:read_property(utils:encode(URI)),
  Json = property2json(Property),
  Req:ok({"text/plain;charset=utf-8", Json});

respond([TypeID, ItemID], _, Req) ->
  Item = store:read_item(utils:encode(ItemID)),
  Json = item2json(Item),
  Req:ok({"text/plain;charset=utf-8", Json}).

item2json(Item) -> mochijson2:encode(item_struct(Item)).

type2json(Type) -> mochijson2:encode(type_struct(Type)).

property2json(Property) -> mochijson2:encode(property_struct(Property)).

list2json(URIs) -> mochijson2:encode(URIs).

itemlist2json(Items) ->
  ItemStructs = [item_struct(Item) || Item <- Items],
  mochijson2:encode(ItemStructs).

item_struct(Item) ->
  {struct, [
    {"uri", Item#item.uri},
    {"label", Item#item.label},
    {"types", Item#item.types},
    {"properties", Item#item.properties}
  ]}.

type_struct(Type) ->
  {struct, [
    {"uri", Type#type.uri},
    {"label", Type#type.label},
    {"types", Type#type.types},
    {"parents", Type#type.parents},
    {"properties", Type#type.properties},
    {"legal_properties", Type#type.legal_properties}
  ]}.

property_struct(Property) ->
  {struct, [
    {"uri", Property#property.uri},
    {"label", Property#property.label},
    {"types", Property#property.types},
    {"properties", Property#property.properties},
    {"ranges", Property#property.ranges},
    {"arity", Property#property.arity},
    {"inverse", Property#property.inverse}
  ]}.