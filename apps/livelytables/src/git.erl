%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc HTTP server
%%% @end
%%%-------------------------------------------------------------------

-module(git).
-export([reset/0, git/1, transaction/1, write/1, read_row/1, read_table/1, read_column/1]).
-include("../include/records.hrl").
-include("../include/config.hrl").


write(Row=#row{}) -> write_row(Row);
write(Table=#table{}) -> write_row(utils:table2row(Table));
write(Column=#column{}) -> write_row(utils:column2row(Column)).

write_row(Row = #row{uri=URI}) ->
  utils:write_file(row_path(URI), utils:row2columnlist(Row)).  

read_row(URI) ->
  read(row_path(URI)).

read_table(URI) ->
  utils:row2table(read_row(URI)).

read_column(URI) ->
  utils:row2column(read_row(URI)).

read(Path) ->
  {ok, ColumnList} = utils:read_file(Path),
  utils:columnlist2row(ColumnList).

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