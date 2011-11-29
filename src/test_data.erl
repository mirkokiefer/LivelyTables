-module(test_data).
-export([core_tables/0, core_coloumns/0, tables/0]).
-export([rows/0, coloumns/0, rows_updated/0, invalid_rows/0, invalid_rows_updated/0]).

-include("../include/records.hrl").


core_tables() ->
  Row = #table{uri= ?ROW, label= <<"Row">>, parents=[], legal_coloumns=[
    ?COLOUMN_LABEL
  ]},
  Table = #table{uri= ?TABLE, label= <<"Table">>, legal_coloumns=[
    ?COLOUMN_LEGALCOLOUMNS,
    ?COLOUMN_PARENTS
  ]},
  Coloumn = #table{uri= ?COLOUMN, label= <<"Coloumn">>,
    legal_coloumns=[
      ?COLOUMN_RANGE,
      ?COLOUMN_ARITY
    ]
  },
  [Row, Table, Coloumn].

core_coloumns() ->
  Label = #coloumn{uri= ?COLOUMN_LABEL, label= <<"Label">>, range=?COLOUMN_TYPE_STRING},
  Parents = #coloumn{uri= ?COLOUMN_PARENTS, label= <<"Parents">>, range=?TABLE, arity=?ARITY_MANY},
  LegalColoumns = #coloumn{uri= ?COLOUMN_LEGALCOLOUMNS, label= <<"Legal Coloumns">>,
    range=?COLOUMN, arity=?ARITY_MANY},
  Range = #coloumn{uri=?COLOUMN_RANGE, label= <<"Range">>, range=?TABLE, arity=?ARITY_ONE},
  Arity = #coloumn{uri=?COLOUMN_ARITY, label= <<"Arity">>, range=?COLOUMN_TYPE_STRING},
  Inverse = #coloumn{uri=?COLOUMN_INVERSE, label= <<"Inverse">>, range=?COLOUMN, optional=true},
  Optional = #coloumn{uri=?COLOUMN_OPTIONAL, label= <<"Optional">>, range=?COLOUMN_TYPE_BOOLEAN},
  [Label, Parents, LegalColoumns, Range, Arity, Inverse, Optional].

tables() ->
  Person = #table{uri= uri(?TABLE, <<"person">>), label= <<"Person">>, legal_coloumns=[uri(?COLOUMN, <<"age">>)]},
  Employee = #table{uri= uri(?TABLE, <<"employee">>), label= <<"Employee">>, parents=[uri(?TABLE, <<"person">>)],
    legal_coloumns=[uri(?COLOUMN, <<"salary">>), uri(?COLOUMN, <<"boss">>)]},
  Manager = #table{uri= uri(?TABLE, <<"manager">>), label= <<"Manager">>, parents=[uri(?TABLE, <<"employee">>)],
    legal_coloumns=[uri(?COLOUMN, <<"manages">>)]},
  [Person, Employee, Manager].

rows() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), label= <<"Paul">>, coloumns=[
    {uri(?COLOUMN, <<"age">>), 30},
    {uri(?COLOUMN, <<"salary">>), 5000}
  ]},
  Jim = #row{uri= uri(<<"manager">>, <<"jim">>), label= <<"Jim">>, coloumns=[
    {uri(?COLOUMN, <<"age">>), 40},
    {uri(?COLOUMN, <<"salary">>), 10000},
    {uri(?COLOUMN, <<"manages">>), [<<"paul">>]}
  ]},
  [Paul, Jim].

rows_updated() ->
  UpdatedPaul = #row{uri= uri(<<"employee">>, <<"paul">>), coloumns=[
    {<<"boss">>, <<"jim">>}
  ]},
  [UpdatedPaul].

coloumns() ->
  Manages = #coloumn{uri= uri(?COLOUMN, <<"manages">>), label= <<"Manages">>, range= uri(?TABLE, <<"employee">>),
    arity=?ARITY_MANY, optional=true},
  Boss = #coloumn{uri= uri(?COLOUMN, <<"boss">>), label= <<"Boss">>, range= uri(?TABLE, <<"manager">>),
    inverse= uri(?COLOUMN, <<"manages">>), optional=true},
  Salary = #coloumn{uri= uri(?COLOUMN, <<"salary">>), label= <<"Salary">>, range=?COLOUMN_TYPE_NUMBER},
  Age = #coloumn{uri= uri(?COLOUMN, <<"age">>), label= <<"Age">>, range=?COLOUMN_TYPE_NUMBER},
  [Manages, Boss, Salary, Age].

invalid_rows() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), label= <<"Paul">>, coloumns=[
    {uri(?COLOUMN, <<"age">>), <<"40">>},
    {uri(?COLOUMN, <<"salary">>), 5000},
    {uri(?COLOUMN, <<"bosss">>), uri(<<"manager">>, <<"jim">>)}
  ]},
  [Paul].

invalid_rows_updated() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), coloumns=[
    {uri(?COLOUMN, <<"bosss">>), <<"jim">>}
  ]},
  [Paul].

uri(Table, Row) ->
  #row_uri{db=?TEST_DB, table=Table, row=Row}.