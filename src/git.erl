-module(git).
-export([reset/0, git/1, transaction/1, write/1, read_item/1, read_type/1, read_property/1]).
-include("../include/records.hrl").
-include("../include/config.hrl").


write(Item=#item{}) -> write_item(Item);
write(Type=#type{}) -> write_item(utils:type2item(Type));
write(Property=#property{}) -> write_item(utils:property2item(Property)).

write_item(Item = #item{uri=URI}) ->
  utils:write_file(item_path(URI), utils:item2propertylist(Item)).  

read_item(URI) ->
  read(item_path(URI)).

read_type(URI) ->
  utils:item2type(read_item(URI)).

read_property(URI) ->
  utils:item2property(read_item(URI)).

read(Path) ->
  {ok, PropertyList} = utils:read_file(Path),
  utils:propertylist2item(PropertyList).

transaction(Fun) ->
  Value = Fun(),
  commit(),
  Value.

commit() ->
  cmds([
    git("add -A"),
    git("commit -m \"write items\"")
  ]).

reset() ->
  Fun = fun() ->
    cmds([
      "rm -R -f " ++ ?GIT_STORE_PATH,
      "mkdir " ++ ?GIT_STORE_PATH,
      git("init")
    ])
  end,
  transaction(Fun).

% utility functions
cmds([Last]) -> cmd(Last);
cmds([First|Rest]) ->
  cmd(First),
  cmds(Rest).

cmd(Command) -> os:cmd(Command).

git(Command) ->
  "git --git-dir=" ++ ?GIT_STORE_PATH ++ ".git --work-tree=" ++ ?GIT_STORE_PATH ++ " " ++ Command.


item_path(URI) -> ?GIT_STORE_PATH ++ bitstring_to_list(URI) ++ ".txt".