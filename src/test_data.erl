-module(test_data).
-export([core_tables/0, core_coloumns/0, set_coloumns/0, set_tables/0, tables/0, composite_rows/0]).
-export([rows/0, coloumns/0, rows_updated/0, invalid_rows/0, invalid_rows_updated/0, composite_rows2/0]).
-export([sample_set/0, record_set/0]).

-include("../include/records.hrl").


core_tables() ->
  Row = #table{uri= ?ROW, label= <<"Row">>, parents=[], legal_coloumns=[
    ?COLOUMN_LABEL,
    ?COLOUMN_TABLES
  ]},
  Table = #table{uri= ?TABLE, label= <<"Table">>, tables=[?TABLE], legal_coloumns=[
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
  Tables = #coloumn{uri= ?COLOUMN_TABLES, label= <<"Tables">>, range=?TABLE, arity=?ARITY_MANY},
  Parents = #coloumn{uri= ?COLOUMN_PARENTS, label= <<"Parents">>, range=?TABLE, arity=?ARITY_MANY},
  LegalColoumns = #coloumn{uri= ?COLOUMN_LEGALCOLOUMNS, label= <<"Legal Coloumns">>,
    range=?COLOUMN, arity=?ARITY_MANY},
  Range = #coloumn{uri=?COLOUMN_RANGE, label= <<"Range">>, range=?TABLE, arity=?ARITY_ONE},
  Arity = #coloumn{uri=?COLOUMN_ARITY, label= <<"Arity">>, range=?COLOUMN_TYPE_STRING},
  Inverse = #coloumn{uri=?COLOUMN_INVERSE, label= <<"Inverse">>, range=?COLOUMN, optional=true},
  Optional = #coloumn{uri=?COLOUMN_OPTIONAL, label= <<"Optional">>, range=?COLOUMN_TYPE_BOOLEAN},
  [Label, Tables, Parents, LegalColoumns, Range, Arity, Inverse, Optional].

tables() ->
  Person = #table{uri= <<"person">>, label= <<"Person">>, legal_coloumns=[<<"age">>]},
  Employee = #table{uri= <<"employee">>, label= <<"Employee">>, parents=[<<"person">>],
    legal_coloumns=[<<"salary">>, <<"boss">>]},
  Manager = #table{uri= <<"manager">>, label= <<"Manager">>, parents=[<<"employee">>],
    legal_coloumns=[<<"manages">>]},
  [Person, Employee, Manager].

composite_rows() ->
  RealEstate = #table{uri= <<"real_estate">>, label= <<"Real Estate">>, legal_coloumns=[
    #coloumn{uri= <<"accomodates">>, label= <<"accomodates">>, range= <<"person">>, arity= <<"many">>}
  ]},
  Company = #table{uri= <<"company">>, label= <<"Company">>, legal_coloumns=[
    #coloumn{uri= <<"company_owns">>, label= <<"owns">>, range= RealEstate, optional=true}
  ]},
  SomeCompany = #row{uri= <<"some_company">>, label= <<"Some Company">>, tables=[Company], coloumns=[
    {<<"company_owns">>, #row{label= <<"A House">>, tables=[<<"real_estate">>], coloumns=[
      {<<"accomodates">>, #row{uri= <<"bob">>, label= <<"Bob">>, tables=[<<"person">>], coloumns=[{<<"age">>, 20}]}}
    ]}}
  ]},
  [SomeCompany].

rows() ->
  Paul = #row{uri= <<"paul">>, label= <<"Paul">>, tables=[<<"employee">>], coloumns=[
    {<<"age">>, 30},
    {<<"salary">>, 5000}
  ]},
  Jim = #row{uri= <<"jim">>, label= <<"Jim">>, tables=[<<"manager">>], coloumns=[
    {<<"age">>, 40},
    {<<"salary">>, 10000},
    {<<"manages">>, [<<"paul">>]}
  ]},
  [Paul, Jim].

rows_updated() ->
  UpdatedPaul = #row{uri= <<"paul">>, coloumns=[
    {<<"boss">>, <<"jim">>}
  ]},
  [UpdatedPaul].

coloumns() ->
  Manages = #coloumn{uri= <<"manages">>, label= <<"Manages">>, range= <<"employee">>,
    arity=?ARITY_MANY, optional=true},
  Boss = #coloumn{uri= <<"boss">>, label= <<"Boss">>, range= <<"manager">>, inverse= <<"manages">>, optional=true},
  Salary = #coloumn{uri= <<"salary">>, label= <<"Salary">>, range=?COLOUMN_TYPE_NUMBER},
  Age = #coloumn{uri= <<"age">>, label= <<"Age">>, range=?COLOUMN_TYPE_NUMBER},
  [Manages, Boss, Salary, Age].

invalid_rows() ->
  Paul = #row{uri= <<"paul">>, label= <<"Paul">>, tables=[<<"employee">>], coloumns=[
    {<<"age">>, <<"40">>},
    {<<"salary">>, 5000},
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

invalid_rows_updated() ->
  Paul = #row{uri= <<"paul">>, coloumns=[
    {<<"bosss">>, <<"jim">>}
  ]},
  [Paul].

composite_rows2() ->
  Alex = #row{uri= <<"alex">>, label= <<"Alex">>, tables=[<<"employee">>], coloumns=[
    {<<"age">>, 50},
    {<<"salary">>, 2000},
    {<<"boss">>, #row{uri= <<"jack">>, label= <<"Jack">>, tables=[<<"manager">>], coloumns=[
      {<<"age">>, 40},
      {<<"salary">>, 10000}
    ]}}
  ]},
  Fred = #row{uri= <<"fred">>, label= <<"Fred">>, tables=[<<"employee">>], coloumns=[
    {<<"age">>, 20},
    {<<"salary">>, 2500},
    {<<"boss">>, #row{label= <<"George">>, tables=[<<"manager">>], coloumns=[
      {<<"age">>, 60},
      {<<"salary">>, 12000}
    ]}}
  ]},
  [Alex, Fred].
