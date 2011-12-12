-module(local_stores).

-export([create_db/1, get_db/1]).

-include("../include/records.hrl").

create_db(DB) ->
  Store = get_db(DB),
  Store:reset(),
  bootstrap_db(Store),
  Store.

get_db(DB) ->
  DBName = binary_to_list(DB),
  DBRows = list_to_atom(DBName ++ "_rows"),
  DBRows2Table = list_to_atom(DBName ++ "_rows2table"),
  DBTableIncludes = list_to_atom(DBName ++ "_table_includes"),
  PluggableStore = pluggable_store:new(DBRows, DBRows2Table, DBTableIncludes),
  store:new(DB, PluggableStore).
  
bootstrap_db(Store) ->
  Store:transaction(fun() -> Store:write(core_tables(Store:name())) end).

% per database meta data
core_tables(DB) ->
  RowURI = ?ROW#row_uri{db=DB},
  Row = #table{uri= RowURI, label= <<"Row">>, parents=[?ROW]},
  Table = #table{uri=?TABLE#row_uri{db=DB}, label= <<"Table">>, parents=[?TABLE, RowURI]},
  Coloumn = #table{uri=?COLOUMN#row_uri{db=DB}, label= <<"Coloumn">>, parents=[?COLOUMN, RowURI]},
  [Row, Table, Coloumn].