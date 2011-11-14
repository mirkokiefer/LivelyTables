-module(git_test).
-export([run/0]).

-include("../include/records.hrl").

run() ->
  write_read_file(),
  {ok, success}.

write_read_file() ->
  T = fun() ->
    Jim = store_interface:read_item(<<"jim">>),
    git:write(Jim),
    Jim == git:read_item(<<"jim">>)
  end,
  true = git:transaction(T).