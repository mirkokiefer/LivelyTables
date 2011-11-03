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
  Type = store_interface:read_type(utils:encode(TypeID)),
  Req:ok({"text/plain;charset=utf-8", utils:json(Type)});

respond([TypeID], _, Req) ->
  ItemURIs = store:read_items_of_type(utils:encode(TypeID)),
  Req:ok({"text/plain;charset=utf-8", utils:json(ItemURIs)});

respond([TypeID, "_full"], _, Req) ->
  io:format("~p~n", [TypeID]),
  ItemURIs = store:read_items_of_type(utils:encode(TypeID)),
  io:format("~p~n", [ItemURIs]),
  Items = [store_interface:read_item(ItemURI, utils:encode(TypeID)) || ItemURI <- ItemURIs],
  Req:ok({"text/plain;charset=utf-8", utils:json(Items)});

respond(["property", URI], _, Req) ->
  Property = store_interface:read_property(utils:encode(URI)),
  Req:ok({"text/plain;charset=utf-8", utils:json(Property)});

respond([TypeID, ItemID], _, Req) ->
  Item = store_interface:read_item(utils:encode(ItemID), utils:encode(TypeID)),
  io:format("~p~n", [Item]),
  Req:ok({"text/plain;charset=utf-8", utils:json(Item)}).