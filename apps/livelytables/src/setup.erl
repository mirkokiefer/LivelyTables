-module(setup).

-export([reset/0, ensure/0]).
-export([meta_tables/0, meta_columns/0]).

-include("../include/records.hrl").

% deletes and re-creates the schema
reset() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),
  mnesia:start(),
  bootstrap_meta().

ensure() ->
  mnesia:start(),
  Tables = local_stores:table_names(?META_DB),
  case utils:is_subset(Tables, mnesia:system_info(tables)) of
    true -> ok;
    false -> reset()
  end.

bootstrap_meta() ->
  Store = local_stores:get_db(?META_DB),
  Store:reset(),
  Store:transaction(fun() -> Store:write(meta_tables() ++ meta_columns()) end),
  {ok, success}.

% global meta data definition
meta_tables() ->
  Row = #table{uri= ?ROW, label= <<"Row">>, parents=[], legal_columns=[
    ?COLUMN_LABEL
  ]},
  Table = #table{uri= ?TABLE, label= <<"Table">>, parents=[?ROW], legal_columns=[
    ?COLUMN_LEGALCOLUMNS,
    ?COLUMN_PARENTS
  ]},
  Column = #table{uri= ?COLUMN, label= <<"Column">>, parents=[?ROW],
    legal_columns=[
      ?COLUMN_RANGE,
      ?COLUMN_ARITY
    ]
  },
  [Row, Table, Column].

meta_columns() ->
  Label = #column{uri= ?COLUMN_LABEL, label= <<"Label">>, range=?COLUMN_TYPE_STRING},
  Parents = #column{uri= ?COLUMN_PARENTS, label= <<"Parents">>, range=?TABLE, arity=?ARITY_MANY},
  LegalColumns = #column{uri= ?COLUMN_LEGALCOLUMNS, label= <<"Legal Columns">>,
    range=?COLUMN, arity=?ARITY_MANY},
  Range = #column{uri=?COLUMN_RANGE, label= <<"Range">>, range=?TABLE, arity=?ARITY_ONE},
  Arity = #column{uri=?COLUMN_ARITY, label= <<"Arity">>, range=?COLUMN_TYPE_STRING},
  Inverse = #column{uri=?COLUMN_INVERSE, label= <<"Inverse">>, range=?COLUMN, optional=true},
  Optional = #column{uri=?COLUMN_OPTIONAL, label= <<"Optional">>, range=?COLUMN_TYPE_BOOLEAN},
  [Label, Parents, LegalColumns, Range, Arity, Inverse, Optional].
