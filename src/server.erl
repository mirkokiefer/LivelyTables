%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc HTTP server
%%% @end
%%%-------------------------------------------------------------------

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

get([TableID], _, Req) ->
  RowURIs = store_interface:read_rows_of_table(TableID),
  send(Req, utils:json(RowURIs));

get([<<"table">>, <<"_full">>], _, Req) ->
  TableURIs = store_interface:read_rows_of_table(?TABLE),
  Tables = [store_interface:read_table(TableURI) || TableURI <- TableURIs],
  send(Req, utils:json(Tables));

get([TableID, <<"_full">>], _, Req) ->
  RowURIs = store_interface:read_rows_of_table(TableID),
  Rows = [store_interface:read_row(RowURI, TableID) || RowURI <- RowURIs],
  send(Req, utils:json(Rows));

get([<<"table">>, TableID], _, Req) ->
  Table = store_interface:read_table(TableID),
  send(Req, utils:json(Table));

get([<<"_file">>|FileTokens], _, Req) ->
  Path = lists:flatten(["/" ++ binary_to_list(Directory) || Directory <- FileTokens]),
  Directory = filename:dirname(Path),
  Req:serve_file(filename:basename(Path), filename:absname("../www/" ++ Directory));

get([TableID, RowID], _, Req) ->
  Row = store_interface:read_row(RowID, TableID),
  send(Req, utils:json(Row));

get([_TableID, _RowID, <<"html">>], _, Req) ->
  Path = "../www/",
  Req:serve_file("ui.html", filename:absname(Path));

get([_TableID, _RowID, Attachment], _, Req) ->
  Req:serve_file(binary_to_list(Attachment), filename:absname("../www")).

put([<<"table">>, TableID], _, Body, Req) ->
  Table = utils:json2table(Body),
  NewTable = Table#table{uri=TableID},
  {atomic, Response} = t(fun() -> store_interface:write_table(NewTable) end),
  send(Req, valid2json(Response));

put([TableID, RowID], _, Body, Req) ->
  Row = utils:json2row(Body),
  NewRow = Row#row{uri=RowID},
  {atomic, Response} = t(fun() -> store_interface:write_row(NewRow, TableID) end),
  send(Req, valid2json(Response)).

valid2json({ok, success}) -> json({struct, [{success, true}]});
valid2json({error, Errors}) -> json({struct, [{success, false}, {errors,Errors}]}).

send(Req, Reply) -> Req:ok({"text/plain;charset=utf-8", Reply}).

json(Term) -> mochijson2:encode(Term).

t(Fun) -> store_interface:transaction(Fun).