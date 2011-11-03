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
  Parameters = lists:sort(Req:parse_qs()),
  case Req:get(method) of
    'GET' -> get(Tokens, Parameters, Req);
    'PUT' -> put(Tokens, Parameters, Req)
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

put([<<"type">>, TypeID], _, Req) ->
  Term = mochijson2:decode(Req:recv_body()),
  log(Term),
  log(utils:json2item(Term)),
  Type = utils:json2type(Term),
  log(Type),
  NewType = Type#type{uri=TypeID},
  Response = case store_interface:write_type(NewType) of
    {ok, success} -> json({struct, [{success, true}]});
    {error, Errors} -> json({struct, [{success, false}, {errors,Errors}]})
  end,
  send(Req, Response);

put([TypeID, ItemID], _, Req) ->
  Term = mochijson2:decode(Req:recv_body()),
  Item = utils:json2item(Term),
  NewItem = Item#item{uri=ItemID},
  Response = case store_interface:write_item(NewItem, TypeID) of
    {ok, success} -> json({struct, [{success, true}]});
    {error, Errors} -> json({struct, [{success, false}, {errors,Errors}]})
  end,
  send(Req, Response).

log(Message) -> io:format("~p~n", [Message]).

send(Req, Reply) -> Req:ok({"text/plain;charset=utf-8", Reply}).

json(Term) -> mochijson2:encode(Term).