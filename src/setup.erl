-module(setup).

-export([start/0, stop/0, reset/0]).

start() ->
  mnesia:create_schema([node()]),
  mnesia:start().
  
% deletes and re-creates the schema
reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  start().

stop() -> mnesia:stop().