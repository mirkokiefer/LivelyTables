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

respond(["item", URI], _, Req) ->
  Item = store:read_item(utils:encode(URI)),
  Json = item2json(Item),
  Req:ok({"text/plain;charset=utf-8", Json});

respond(["type", URI], _, Req) ->
  Type = store:read_type(utils:encode(URI)),
  Json = type2json(Type),
  Req:ok({"text/plain;charset=utf-8", Json});

respond(["property", URI], _, Req) ->
  Property = store:read_property(utils:encode(URI)),
  Json = property2json(Property),
  Req:ok({"text/plain;charset=utf-8", Json}).

item2json(#item{uri=URI, label=Label, types=Types, properties=Props}) ->
  mochijson2:encode({struct, [
    {"uri", URI},
    {"label", Label},
    {"types", Types},
    {"properties", Props}
  ]}).

type2json(Type) ->
  mochijson2:encode({struct, [
    {"uri", Type#type.uri},
    {"label", Type#type.label},
    {"types", Type#type.types},
    {"parents", Type#type.parents},
    {"properties", Type#type.properties},
    {"legal_properties", Type#type.legal_properties}
  ]}).

property2json(Property) ->
  mochijson2:encode({struct, [
    {"uri", Property#property.uri},
    {"label", Property#property.label},
    {"types", Property#property.types},
    {"properties", Property#property.properties},
    {"ranges", Property#property.ranges},
    {"arity", Property#property.arity},
    {"inverse", Property#property.inverse}
  ]}).