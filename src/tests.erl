-module(tests).
-export([run/0]).

run() ->
  store:clear(),
  {ok, success} = store_test:run(),
  {ok, success} = store_interface_test:run(),
  {ok, success} = set_interface_test:run(),
  {ok, success} = git_test:run().