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
  Tokens = [utils:encode(Token) || Token <- string:tokens(Req:get(path), "/")],
  Parameters = lists:sort(Req:parse_qs()),
  case Req:get(method) of
    'GET' -> get(Tokens, Parameters, Req);
    'PUT' -> put(Tokens, Parameters, mochijson2:decode(Req:recv_body()), Req)
  end.

get([TypeID], _, Req) ->
  ItemURIs = store:read_items_of_type(TypeID),
  send(Req, utils:json(ItemURIs));

get([<<"type">>, <<"_full">>], _, Req) ->
  TypeURIs = store:read_items_of_type(?TYPE),
  Types = [store_interface:read_type(TypeURI) || TypeURI <- TypeURIs],
  send(Req, utils:json(Types));

get([TypeID, <<"_full">>], _, Req) ->
  ItemURIs = store:read_items_of_type(TypeID),
  Items = [store_interface:read_item(ItemURI, TypeID) || ItemURI <- ItemURIs],
  send(Req, utils:json(Items));

get([<<"type">>, TypeID], _, Req) ->
  Type = store_interface:read_type(TypeID),
  send(Req, utils:json(Type));

get([<<"property">>, URI], _, Req) ->
  Property = store_interface:read_property(URI),
  send(Req, utils:json(Property));

get([TypeID, ItemID], _, Req) ->
  Item = store_interface:read_item(ItemID, TypeID),
  send(Req, utils:json(Item));

get([_TypeID, _ItemID, <<"html">>], _, Req) ->
  Path = "../www/",
  Req:serve_file("ui.html", filename:absname(Path));

get([_TypeID, _ItemID, Attachment], _, Req) ->
  Req:serve_file(binary_to_list(Attachment), filename:absname("../www")).

put([<<"type">>, TypeID], _, Body, Req) ->
  Type = utils:json2type(Body),
  NewType = Type#type{uri=TypeID},
  Response = valid2json(store_interface:write_type(NewType)),
  send(Req, Response);

put([<<"property">>, PropertyID], _, Body, Req) ->
  Property = utils:json2property(Body),
  NewProperty = Property#property{uri=PropertyID},
  Response = valid2json(store_interface:write_property(NewProperty)),
  send(Req, Response);

put([TypeID, ItemID], _, Body, Req) ->
  Item = utils:json2item(Body),
  NewItem = Item#item{uri=ItemID},
  Response = valid2json(store_interface:write_item(NewItem, TypeID)),
  send(Req, Response).

valid2json({ok, success}) -> json({struct, [{success, true}]});
valid2json({error, Errors}) -> json({struct, [{success, false}, {errors,Errors}]}).

send(Req, Reply) -> Req:ok({"text/plain;charset=utf-8", Reply}).

json(Term) -> mochijson2:encode(Term).
