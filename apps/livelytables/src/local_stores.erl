-module(local_stores).

-export([create_db/1, get_db/1, table_names/1]).

-include("../include/records.hrl").

create_db(DB) ->
  Store = get_db(DB),
  Store:reset(),
  bootstrap_db(Store),
  Store.

get_db(DB) ->
  [DBRows, DBRows2Table, DBTableIncludes] = table_names(DB),
  PluggableStore = pluggable_store:new(DBRows, DBRows2Table, DBTableIncludes),
  store:new(DB, PluggableStore).

table_names(DB) ->
  DBName = binary_to_list(DB),
  DBRows = list_to_atom(DBName ++ "_rows"),
  DBRows2Table = list_to_atom(DBName ++ "_rows2table"),
  DBTableIncludes = list_to_atom(DBName ++ "_table_includes"),
  [DBRows, DBRows2Table, DBTableIncludes].

bootstrap_db(Store) ->
  Store:transaction(fun() -> Store:write(core_tables(Store:name())) end).

% per database meta data
core_tables(DB) ->
  RowURI = ?ROW#row_uri{db=DB},
  Row = #table{uri= RowURI, label= <<"Row">>, parents=[?ROW]},
  Table = #table{uri=?TABLE#row_uri{db=DB}, label= <<"Table">>, parents=[?TABLE, RowURI]},
  Column = #table{uri=?COLUMN#row_uri{db=DB}, label= <<"Column">>, parents=[?COLUMN, RowURI]},
  [Row, Table, Column].