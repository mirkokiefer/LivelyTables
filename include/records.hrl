
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

% Set Types
-define(SET, <<"set">>).
-define(ROW_LIST, <<"row_list">>).
-define(PROJECT_SET, <<"project_set">>).
-define(SET_OPERATION, <<"set_operation">>).
-define(INTERSECTION, <<"intersection">>).
-define(UNION, <<"union">>).

-define(TRANSFORM_SET, <<"set_transform">>).
-define(TRANSFORM_CELLS_AS_TABLE, <<"transform_cells_as_table">>).
-define(TRANSFORM_COLOUMNS_AS_TABLE, <<"transform_coloumns_as_table">>).
-define(TRANSFORM_ROWS_WITH_COLOUMNS, <<"transform_rows_with_coloumns">>).
-define(TRANSFORM_ROWS_IN_TABLES, <<"transform_rows_in_tables">>).

-define(FILTER, <<"filter">>).

-define(CONDITION, <<"condition">>).
-define(COLOUMN_EXISTS_CONDITION, <<"coloumn_exists_condition">>).
-define(VALUE_CONDITION, <<"value_condition">>).
-define(VALUE_CONDITION_EQUALS, <<"value_condition_equals">>).

% Set Properties
-define(COLOUMN_ROWS, <<"coloumn_rows">>).
-define(COLOUMN_SET, <<"coloumn_set">>).
-define(COLOUMN_SETS, <<"coloumn_sets">>).
-define(COLOUMN_COLOUMN_SET, <<"coloumn_coloumn_set">>).
-define(COLOUMN_CONDITIONS, <<"coloumn_conditions">>).
-define(COLOUMN_VALUE, <<"coloumn_value">>).

-record(row, {uri, label, tables=[], coloumns=[]}).
-record(table, {uri, label, tables=[?TABLE], coloumns=[], parents=[?ROW], legal_coloumns=[]}).
-record(coloumn, {uri, label, tables=[?COLOUMN], coloumns=[], range=[?ROW], arity=?ARITY_ONE,
  inverse, optional=false}).

% Set records
-record(union, {sets}).
-record(intersection, {sets}).
-record(filter, {set, conditions}).
-record(rows2values, {rows, coloumns}).
-record(rows2coloumns, {rows}).
-record(coloumns2rows, {coloumns}).
-record(tables2rows, {tables}).

-record(value_equals, {coloumns, value}).
-record(coloumn_exists, {coloumns}).
