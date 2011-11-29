-module(local_stores).

-export([get/1]).

get(DB) ->
  DBName = binary_to_list(DB),
  DBRows = list_to_atom(DBName ++ "_rows"),
  DBRows2Table = list_to_atom(DBName ++ "_rows2table"),
  DBTableIncludes = list_to_atom(DBName ++ "_table_includes"),
  PluggableStore = pluggable_store:new(DBRows, DBRows2Table, DBTableIncludes),
  store:new(PluggableStore).