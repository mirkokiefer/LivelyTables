%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc HTTP server
%%% @end
%%%-------------------------------------------------------------------

-module(server).

-export([start_link/0]).

% mochiweb callbacks
-export([loop/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-behaviour(gen_server).

-include("../include/records.hrl").

-define(HTTP_OPTS, [
            {loop, {?MODULE, loop}},
            {port, 8080},
            {name, http_sapiento}
            ]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, _Http} = mochiweb_http:start(?HTTP_OPTS),
  {ok, []}.

handle_call(hello, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(http_routing),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

loop(Req) ->
  Tokens = [utils:encode(Token) || Token <- string:tokens(Req:get(path), "/")],
  Parameters = lists:sort(Req:parse_qs()),
  case Req:get(method) of
    'GET' -> get(Tokens, Parameters, Req);
    'PUT' -> put(Tokens, Parameters, mochijson2:decode(Req:recv_body()), Req)
  end.

get([<<"_file">>|FileTokens], _, Req) ->
  Path = lists:flatten(["/" ++ binary_to_list(Directory) || Directory <- FileTokens]),
  Directory = filename:dirname(Path),
  Req:serve_file(filename:basename(Path), filename:absname("../www/" ++ Directory));

get([DB], _, Req) ->
  RowURIs = global_interface:read_tables(#db_uri{db=DB}),
  send(Req, utils:json(RowURIs));

get([DB, TableID], _, Req) ->
  RowURIs = global_interface:read_rows(#row_uri{db=DB, table=?TABLE_ID, row=TableID}),
  send(Req, utils:json(RowURIs));

get([DB, TableID, <<"_full">>], _, Req) ->
  RowURIs = global_interface:read_rows(#row_uri{db=DB, table=?TABLE_ID, row=TableID}),
  Rows = [global_interface:read_row(RowURI) || RowURI <- RowURIs],
  send(Req, utils:json(Rows));

get([DB, TableID, RowID], _, Req) ->
  Row = global_interface:read_row(#row_uri{db=DB, table=TableID, row=RowID}),
  send(Req, utils:json(Row));

get([_DB, _TableID, _RowID, <<"html">>], _, Req) ->
  Path = "../www/",
  Req:serve_file("ui.html", filename:absname(Path)).

put([DB, TableID, RowID], _, Body, Req) ->
  Row = utils:json2row(Body),
  NewRow = Row#row{uri=#row_uri{db=DB, table=TableID, row=RowID}},
  {atomic, Response} = t(fun() -> global_interface:write_row(NewRow) end),
  send(Req, valid2json(Response)).

valid2json({ok, success}) -> json({struct, [{success, true}]});
valid2json({error, Errors}) -> json({struct, [{success, false}, {errors,Errors}]}).

send(Req, Reply) -> Req:ok({"text/plain;charset=utf-8", Reply}).

json(Term) -> mochijson2:encode(Term).

t(Fun) -> store_interface:transaction(Fun).