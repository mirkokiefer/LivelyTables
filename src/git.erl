-module(git).
-export([reset/0, git/1, transaction/1, write/1, read_row/1, read_table/1, read_coloumn/1]).
-include("../include/records.hrl").
-include("../include/config.hrl").


write(Row=#row{}) -> write_row(Row);
write(Table=#table{}) -> write_row(utils:table2row(Table));
write(Coloumn=#coloumn{}) -> write_row(utils:coloumn2row(Coloumn)).

write_row(Row = #row{uri=URI}) ->
  utils:write_file(row_path(URI), utils:row2coloumnlist(Row)).  

read_row(URI) ->
  read(row_path(URI)).

read_table(URI) ->
  utils:row2table(read_row(URI)).

read_coloumn(URI) ->
  utils:row2coloumn(read_row(URI)).

read(Path) ->
  {ok, ColoumnList} = utils:read_file(Path),
  utils:coloumnlist2row(ColoumnList).

transaction(Fun) ->
  Value = Fun(),
  commit(),
  Value.

commit() ->
  cmds([
    git("add -A"),
    git("commit -m \"write rows\"")
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


row_path(#row_uri{row=Row}) -> ?GIT_STORE_PATH ++ binary_to_list(Row) ++ ".txt".