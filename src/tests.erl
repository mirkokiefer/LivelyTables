-module(tests).
-export([run/0]).

run() -> utils:time_seconds(fun tests/0).

tests() ->
  store:clear(),
  git:reset(),
  {ok, success} = store_test:run(),
  {ok, success} = store_interface_test:run(),
  {ok, success} = set_interface_test:run(),
  {ok, success} = git_test:run().