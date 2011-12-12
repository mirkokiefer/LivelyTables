-module(test_data).
-export([tables/0]).
-export([rows/0, coloumns/0, rows_updated/0, invalid_rows/0, invalid_rows_updated/0]).

-include("../include/records.hrl").

tables() ->
  Person = #table{uri= uri(?TABLE_ID, <<"person">>), label= <<"Person">>,
    legal_coloumns=[uri(?COLOUMN_ID, <<"age">>)]},
  Employee = #table{uri= uri(?TABLE_ID, <<"employee">>), label= <<"Employee">>,
    parents=[uri(?TABLE_ID, <<"person">>)],
    legal_coloumns=[uri(?COLOUMN_ID, <<"salary">>), uri(?COLOUMN_ID, <<"boss">>)]},
  Manager = #table{uri= uri(?TABLE_ID, <<"manager">>), label= <<"Manager">>,
    parents=[uri(?TABLE_ID, <<"employee">>)],
    legal_coloumns=[uri(?COLOUMN_ID, <<"manages">>)]},
  [Person, Employee, Manager].

rows() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), label= <<"Paul">>, coloumns=[
    {uri(?COLOUMN_ID, <<"age">>), 30},
    {uri(?COLOUMN_ID, <<"salary">>), 5000}
  ]},
  Jim = #row{uri= uri(<<"manager">>, <<"jim">>), label= <<"Jim">>, coloumns=[
    {uri(?COLOUMN_ID, <<"age">>), 40},
    {uri(?COLOUMN_ID, <<"salary">>), 10000},
    {uri(?COLOUMN_ID, <<"manages">>), [uri(<<"employee">>, <<"paul">>)]}
  ]},
  [Paul, Jim].

rows_updated() ->
  UpdatedPaul = #row{uri= uri(<<"employee">>, <<"paul">>), coloumns=[
    {uri(?COLOUMN_ID, <<"age">>), 30},
    {uri(?COLOUMN_ID, <<"salary">>), 5000},
    {uri(?COLOUMN_ID, <<"boss">>), uri(<<"manager">>, <<"jim">>)}
  ]},
  [UpdatedPaul].

coloumns() ->
  Manages = #coloumn{uri= uri(?COLOUMN_ID, <<"manages">>), label= <<"Manages">>, range= uri(?TABLE_ID, <<"employee">>),
    arity=?ARITY_MANY, optional=true},
  Boss = #coloumn{uri= uri(?COLOUMN_ID, <<"boss">>), label= <<"Boss">>, range= uri(?TABLE_ID, <<"manager">>),
    inverse= uri(?COLOUMN_ID, <<"manages">>), optional=true},
  Salary = #coloumn{uri= uri(?COLOUMN_ID, <<"salary">>), label= <<"Salary">>, range=?COLOUMN_TYPE_NUMBER},
  Age = #coloumn{uri= uri(?COLOUMN_ID, <<"age">>), label= <<"Age">>, range=?COLOUMN_TYPE_NUMBER},
  [Manages, Boss, Salary, Age].

invalid_rows() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), label= <<"Paul">>, coloumns=[
    {uri(?COLOUMN_ID, <<"age">>), <<"40">>},
    {uri(?COLOUMN_ID, <<"salary">>), 5000},
    {uri(?COLOUMN_ID, <<"bosss">>), uri(<<"manager">>, <<"jim">>)}
  ]},
  [Paul].

invalid_rows_updated() ->
  Paul = #row{uri= uri(<<"employee">>, <<"paul">>), coloumns=[
    {uri(?COLOUMN_ID, <<"bosss">>), <<"jim">>}
  ]},
  [Paul].

uri(Table, Row) ->
  #row_uri{db=?TEST_DB, table=Table, row=Row}.