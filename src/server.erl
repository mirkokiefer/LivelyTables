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

respond(["item", ItemURI], _, Req) ->
  Item = store:read_item(utils:encode(ItemURI)),
  Json = item2json(Item),
  Req:ok({"text/plain;charset=utf-8", Json}).

item2json(#item{uri=URI, label=Label, types=Types, properties=Props}) ->
  mochijson2:encode({struct, [
    {"uri", URI},
    {"label", Label},
    {"types", Types},
    {"properties", Props}
  ]}).