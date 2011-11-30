-module(bootstrap).

-export([bootstrap_meta/0, bootstrap_db/1, meta_tables/0, meta_coloumns/0]).

-include("../include/records.hrl").

bootstrap_meta() ->
  Store = local_stores:get_db(?META_DB),
  Store:reset(),
  Store:transaction(fun() -> Store:write(meta_tables() ++ meta_coloumns()) end),
  {ok, success}.

bootstrap_db(Store) ->
  Store:transaction(fun() -> Store:write(core_tables(Store:name())) end).

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

% per database meta data
core_tables(DB) ->
  RowURI = ?ROW#row_uri{db=DB},
  Row = #table{uri= RowURI, label= <<"Row">>, parents=[?ROW]},
  Table = #table{uri=?TABLE#row_uri{db=DB}, label= <<"Table">>, parents=[?TABLE, RowURI]},
  Coloumn = #table{uri=?COLOUMN#row_uri{db=DB}, label= <<"Coloumn">>, parents=[?COLOUMN, RowURI]},
  [Row, Table, Coloumn].