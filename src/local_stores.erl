-module(local_stores).

-export([create_db/1, get_db/1]).

create_db(DB) ->
  Store = get_db(DB),
  Store:reset(),
  setup:bootstrap_db(Store),
  Store.

get_db(DB) ->
  DBName = binary_to_list(DB),
  DBRows = list_to_atom(DBName ++ "_rows"),
  DBRows2Table = list_to_atom(DBName ++ "_rows2table"),
  DBTableIncludes = list_to_atom(DBName ++ "_table_includes"),
  PluggableStore = pluggable_store:new(DBRows, DBRows2Table, DBTableIncludes),
  store:new(DB, PluggableStore).