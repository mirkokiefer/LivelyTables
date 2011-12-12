-module(setup).

-export([reset/0, ensure/0]).
-export([meta_tables/0, meta_coloumns/0]).

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
  Store:transaction(fun() -> Store:write(meta_tables() ++ meta_coloumns()) end),
  {ok, success}.

% global meta data definition
meta_tables() ->
  Row = #table{uri= ?ROW, label= <<"Row">>, parents=[], legal_coloumns=[
    ?COLOUMN_LABEL
  ]},
  Table = #table{uri= ?TABLE, label= <<"Table">>, parents=[?ROW], legal_coloumns=[
    ?COLOUMN_LEGALCOLOUMNS,
    ?COLOUMN_PARENTS
  ]},
  Coloumn = #table{uri= ?COLOUMN, label= <<"Coloumn">>, parents=[?ROW],
    legal_coloumns=[
      ?COLOUMN_RANGE,
      ?COLOUMN_ARITY
    ]
  },
  [Row, Table, Coloumn].

meta_coloumns() ->
  Label = #coloumn{uri= ?COLOUMN_LABEL, label= <<"Label">>, range=?COLOUMN_TYPE_STRING},
  Parents = #coloumn{uri= ?COLOUMN_PARENTS, label= <<"Parents">>, range=?TABLE, arity=?ARITY_MANY},
  LegalColoumns = #coloumn{uri= ?COLOUMN_LEGALCOLOUMNS, label= <<"Legal Coloumns">>,
    range=?COLOUMN, arity=?ARITY_MANY},
  Range = #coloumn{uri=?COLOUMN_RANGE, label= <<"Range">>, range=?TABLE, arity=?ARITY_ONE},
  Arity = #coloumn{uri=?COLOUMN_ARITY, label= <<"Arity">>, range=?COLOUMN_TYPE_STRING},
  Inverse = #coloumn{uri=?COLOUMN_INVERSE, label= <<"Inverse">>, range=?COLOUMN, optional=true},
  Optional = #coloumn{uri=?COLOUMN_OPTIONAL, label= <<"Optional">>, range=?COLOUMN_TYPE_BOOLEAN},
  [Label, Parents, LegalColoumns, Range, Arity, Inverse, Optional].
