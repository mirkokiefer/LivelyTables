% URI records
-define(LOCALHOST, <<"">>).
-record(db_uri, {domain=?LOCALHOST, db}).
-record(table_uri, {domain=?LOCALHOST, db, table}).
-record(row_uri, {domain=?LOCALHOST, db, table, row}).


%Database names
-define(META_DB, <<"meta">>).
-define(TEST_DB, <<"testdb">>).

% Core Types
-define(ROW_ID, <<"row">>).
-define(TABLE_ID, <<"table">>).
-define(COLUMN_ID, <<"column">>).
-define(LITERAL_ID, <<"literal">>).

-define(TABLE, #row_uri{db=?META_DB, table= ?TABLE_ID, row= ?TABLE_ID}).
-define(ROW, #row_uri{db=?META_DB, table=?TABLE_ID, row= ?ROW_ID}).
-define(COLUMN,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"column">>}).
-define(LITERAL,  #row_uri{db=?META_DB, table=?TABLE_ID, row= ?COLUMN_TYPE_ID}).
-define(VIEW,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"view">>}).
-define(HIERARCHY,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"hierarchy">>).

% Property Types
-define(COLUMN_TYPE_STRING, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"string">>}).
-define(COLUMN_TYPE_BOOLEAN, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"boolean">>}).
-define(COLUMN_TYPE_NUMBER, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"number">>}).

-define(ARITY_ONE, <<"one">>).
-define(ARITY_MANY, <<"many">>).

% Core Columns
-define(URI_ID, <<"uri">>).
-define(URI, #row_uri{db=?META_DB, table=?COLUMN_ID, row= ?URI_ID}).
-define(COLUMN_LABEL, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"label">>}).
-define(COLUMN_LEGALCOLUMNS, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"legal_columns">>}).
-define(COLUMN_PARENTS, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"parents">>}).
-define(COLUMN_RANGE, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"range">>}).
-define(COLUMN_ARITY, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"arity">>}).
-define(COLUMN_OPTIONAL, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"optional">>}).
-define(COLUMN_INVERSE, #row_uri{db=?META_DB, table=?COLUMN_ID, row= <<"inverse">>}).

% Core Records
-record(row, {uri, label, columns=[]}).
-record(table, {uri, label, columns=[], parents, legal_columns=[]}).
-record(column, {uri, label, columns=[], range=[?ROW], arity=?ARITY_ONE, inverse, optional=false}).

% Database records
-record(db_rows, {row_id, label, columns=[]}).
-record(db_rows2table, {row_id, table_id}).
-record(db_table_includes, {table_id, included_table_id}).