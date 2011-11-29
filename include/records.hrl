
% Property Types
-define(COLOUMN_TYPE_STRING, <<"string">>).
-define(COLOUMN_TYPE_BOOLEAN, <<"boolean">>).
-define(COLOUMN_TYPE_NUMBER, <<"number">>).

-define(ARITY_ONE, <<"one">>).
-define(ARITY_MANY, <<"many">>).

% Core Types
-define(ROW, <<"row">>).
-define(TABLE, <<"table">>).
-define(COLOUMN, <<"coloumn">>).
-define(VIEW, <<"view">>).
-define(HIERARCHY, <<"hierarchy">>).

% Core Properties
-define(URI, <<"uri">>).
-define(COLOUMN_LABEL, <<"label">>).
-define(COLOUMN_TABLES, <<"tables">>).
-define(COLOUMN_LEGALCOLOUMNS, <<"legal_coloumns">>).
-define(COLOUMN_PARENTS, <<"parents">>).
-define(COLOUMN_RANGE, <<"range">>).
-define(COLOUMN_ARITY, <<"arity">>).
-define(COLOUMN_OPTIONAL, <<"optional">>).
-define(COLOUMN_INVERSE, <<"inverse">>).


% Set Properties
-define(COLOUMN_ROWS, <<"coloumn_rows">>).
-define(COLOUMN_SET, <<"coloumn_set">>).
-define(COLOUMN_SETS, <<"coloumn_sets">>).
-define(COLOUMN_COLOUMN_SET, <<"coloumn_coloumn_set">>).
-define(COLOUMN_CONDITIONS, <<"coloumn_conditions">>).
-define(COLOUMN_VALUE, <<"coloumn_value">>).

% Core Records
-record(row, {uri, label, tables=[], coloumns=[]}).
-record(table, {uri, label, tables=[?TABLE], coloumns=[], parents=[?ROW], legal_coloumns=[]}).
-record(coloumn, {uri, label, tables=[?COLOUMN], coloumns=[], range=[?ROW], arity=?ARITY_ONE,
  inverse, optional=false}).



% Database records
-record(db_rows, {uri, label, coloumns=[]}).
-record(db_rows2table, {row, table}).
-record(db_table_includes, {table, included_table}).