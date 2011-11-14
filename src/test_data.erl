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
  Tables = #coloumn{uri= ?COLOUMN_TABLES, label= <<"Tables">>, range=?TABLE, arity=?ARITY_MANY},
  Parents = #coloumn{uri= ?COLOUMN_PARENTS, label= <<"Parents">>, range=?TABLE, arity=?ARITY_MANY},
  LegalProps = #coloumn{uri= ?COLOUMN_LEGALCOLOUMNS, label= <<"Legal Coloumns">>,
    range=?COLOUMN, arity=?ARITY_MANY},
  Range = #coloumn{uri=?COLOUMN_RANGE, label= <<"Range">>, range=?TABLE, arity=?ARITY_ONE},
  Arity = #coloumn{uri=?COLOUMN_ARITY, label= <<"Arity">>, range=?COLOUMN_TYPE_STRING},
  Inverse = #coloumn{uri=?COLOUMN_INVERSE, label= <<"Inverse">>, range=?COLOUMN, optional=true},
  Optional = #coloumn{uri=?COLOUMN_OPTIONAL, label= <<"Optional">>, range=?COLOUMN_TYPE_BOOLEAN},
  [Label, Tables, Parents, LegalProps, Range, Arity, Inverse, Optional].

set_coloumns() ->
  Sets = #coloumn{uri= ?COLOUMN_SETS, label= <<"Sets">>, range=?SET, arity=?ARITY_MANY},
  Set = #coloumn{uri= ?COLOUMN_SET, label= <<"Set">>, range=?SET},
  Rows = #coloumn{uri= ?COLOUMN_ROWS, label= <<"Rows">>, range=?ROW, arity=?ARITY_MANY},

  ColoumnSet = #coloumn{uri= ?COLOUMN_COLOUMN_SET, label= <<"Coloumn set">>, range=?SET},

  Conditions = #coloumn{uri= ?COLOUMN_CONDITIONS, label= <<"Coloumn Conditions">>,
    range=?CONDITION, arity=?ARITY_MANY},
  Value = #coloumn{uri= ?COLOUMN_VALUE, label= <<"Value">>, range=?ROW},

  [Sets, Set, Rows, ColoumnSet, Conditions, Value].


set_tables() ->
  Set = #table{uri= ?SET, label= <<"Set">>, legal_coloumns=[]},
  RowList = #table{uri= ?ROW_LIST, label= <<"Row list">>,
    parents=[?SET], legal_coloumns=[?COLOUMN_ROWS]},
  % a dummy row representing the project set:
  Project = #row{uri= ?PROJECT_SET, tables=[?SET], label= <<"Project set">>},
  [Set, RowList, Project | set_operations() ++ set_transforms() ++ set_filters()].

set_operations() ->
  SetOperation = #table{uri= ?SET_OPERATION, label= <<"Set Operation">>, parents=[?SET],
    legal_coloumns=[?COLOUMN_SETS]},
  Union = #table{uri= ?UNION, label= <<"Union">>, parents=[?SET_OPERATION], legal_coloumns=[]},
  Intersection = #table{uri= ?INTERSECTION, label= <<"Intersection">>, parents=[?SET_OPERATION], legal_coloumns=[]},
  [SetOperation, Union, Intersection].

set_transforms() ->
  SetTransform = #table{uri= ?TRANSFORM_SET, label= <<"Set Transform">>, parents=[?SET], legal_coloumns=[
    ?COLOUMN_SET
  ]},
  RowsToValues = #table{uri= ?TRANSFORM_CELLS_AS_TABLE, label= <<"Rows -> Values">>,
    parents=[?TRANSFORM_SET], legal_coloumns=[?COLOUMN_COLOUMN_SET]},
  RowsToColoumns = #table{uri= ?TRANSFORM_ROWS_WITH_COLOUMNS, label= <<"Rows -> Coloumns">>,
    parents=[?TRANSFORM_SET], legal_coloumns=[]},
  ColoumnsToRows = #table{uri= ?TRANSFORM_ROWS_WITH_COLOUMNS, label= <<"Coloumns -> Rows">>,
    parents=[?TRANSFORM_SET], legal_coloumns=[]},
  TablesToRows = #table{uri= ?TRANSFORM_ROWS_IN_TABLES, label= <<"Tables -> Rows">>,
    parents=[?TRANSFORM_SET], legal_coloumns=[]},

  [SetTransform, RowsToValues, RowsToColoumns, ColoumnsToRows, TablesToRows].

set_filters() ->
  Filter = #table{uri= ?FILTER, label= <<"Filter">>, parents=[?SET], legal_coloumns=[
    ?COLOUMN_SET,
    ?COLOUMN_CONDITIONS
  ]},

  Condition = #table{uri= ?CONDITION, label= <<"Coloumn Condition">>, parents=[?SET], legal_coloumns=[
    ?COLOUMN_COLOUMN_SET
  ]},

  ColoumnExists = #table{uri= ?COLOUMN_EXISTS_CONDITION, label= <<"Coloumn exists">>, parents=[?CONDITION]},
  ValueCondition = #table{uri= ?VALUE_CONDITION, label= <<"Value condition">>, parents=[?CONDITION]},
  Equals = #table{uri= ?VALUE_CONDITION_EQUALS, label= <<"Value equals">>, parents=[?VALUE_CONDITION],
    legal_coloumns=[?COLOUMN_VALUE]},

  [Filter, Condition, ColoumnExists, ValueCondition, Equals].

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

sample_set() ->
  Persons = #row{tables=[?TRANSFORM_ROWS_IN_TABLES], coloumns=[
    {?COLOUMN_SET, row_list([<<"employee">>])}
  ]},
  ColoumnExists = #row{tables=[?COLOUMN_EXISTS_CONDITION], coloumns=[
    {?COLOUMN_COLOUMN_SET, row_list([<<"boss">>])}
  ]},
  ColoumnCondition = #row{tables=[?VALUE_CONDITION_EQUALS], coloumns=[
    {?COLOUMN_COLOUMN_SET, row_list([<<"boss">>])},
    {?COLOUMN_VALUE, <<"jim">>}
  ]},
  BossJim = #row{label= <<"Persons with boss Jim">>, tables=[?FILTER], coloumns=[
    {?COLOUMN_SET, Persons},
    {?COLOUMN_CONDITIONS, [ColoumnExists, ColoumnCondition]}
  ]},
  #row{uri= <<"sample_set">>, label= <<"Sample set">>, tables=[?UNION], coloumns=[
    {?COLOUMN_SETS, [
      BossJim,
      #row{tables=[?ROW_LIST], coloumns=[{?COLOUMN_ROWS, [<<"jim">>]}]}
    ]}
  ]}.

record_set() ->
  Persons = #tables2rows{tables=[<<"employee">>]},
  ColoumnExists = #coloumn_exists{coloumns=[<<"boss">>]},
  ColoumnConditions = #value_equals{coloumns= [<<"boss">>], value= <<"jim">>},
  BossJim = #filter{set=Persons, conditions=[ColoumnExists, ColoumnConditions]},
  #union{sets=[BossJim, [<<"jim">>]]}.

row_list(Rows) ->
  #row{tables=[?ROW_LIST], coloumns=[{?COLOUMN_ROWS, Rows}]}.