-module(git_test).
-export([run/0]).

-include("../include/records.hrl").

run() ->
  write_read_file(),
  write_repo(),
  {ok, success}.

write_read_file() ->
  T = fun() ->
    Jim = store_interface:read_item(<<"jim">>),
    git:write(Jim),
    Jim == git:read_item(<<"jim">>)
  end,
  true = git:transaction(T).

write_repo() ->
  T = fun() ->
    ItemURIs = store:read_items_of_type(?ITEM),
    [git:write(store:read_item(URI)) || URI <- ItemURIs]
  end,
  git:transaction(T).