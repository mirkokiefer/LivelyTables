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
  Tokens = [utils:encode(Token) || Token <- string:tokens(Path, "/")],
  respond(Tokens, lists:sort(Req:parse_qs()), Req).

respond([TypeID], _, Req) ->
  ItemURIs = store:read_items_of_type(TypeID),
  send(Req, utils:json(ItemURIs));

respond([<<"type">>, <<"_full">>], _, Req) ->
  TypeURIs = store:read_items_of_type(?TYPE),
  Types = [store_interface:read_type(TypeURI) || TypeURI <- TypeURIs],
  send(Req, utils:json(Types));

respond([TypeID, <<"_full">>], _, Req) ->
  ItemURIs = store:read_items_of_type(TypeID),
  Items = [store_interface:read_item(ItemURI, TypeID) || ItemURI <- ItemURIs],
  send(Req, utils:json(Items));

respond([<<"type">>, TypeID], _, Req) ->
  Type = store_interface:read_type(TypeID),
  send(Req, utils:json(Type));

respond([<<"property">>, URI], _, Req) ->
  Property = store_interface:read_property(URI),
  send(Req, utils:json(Property));

respond([TypeID, ItemID], _, Req) ->
  Item = store_interface:read_item(ItemID, TypeID),
  send(Req, utils:json(Item));

respond([_TypeID, _ItemID, <<"html">>], _, Req) ->
  Path = "../www/",
  Req:serve_file("ui.html", filename:absname(Path));

respond([TypeID, ItemID, Attachment], _, Req) ->
  %Path = string:join(["../www", binary_to_list(TypeID), binary_to_list(ItemID)], "/"),
  Req:serve_file(binary_to_list(Attachment), filename:absname("../www")).

log(Message) -> io:format("~p~n", [Message]).

send(Req, Reply) -> Req:ok({"text/plain;charset=utf-8", Reply}).