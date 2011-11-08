-module(tests).
-export([run/0]).

run() ->
  store:reset(),
  {ok, success} = store_test:run(),
  {ok, success} = store_interface_test:run().