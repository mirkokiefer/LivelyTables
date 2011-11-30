% URI records
-define(LOCALHOST, <<"">>).
-record(db_uri, {domain=?LOCALHOST, db}).
-record(table_uri, {domain=?LOCALHOST, db, table}).
-record(row_uri, {domain=?LOCALHOST, db, table, row}).


%Database names
-define(META_DB, <<"meta">>).
-define(TEST_DB, <<"testdb">>).

% Core Types
-define(TABLE_ID, <<"table">>).
-define(COLOUMN_ID, <<"coloumn">>).
-define(LITERAL_ID, <<"literal">>).

-define(TABLE, #row_uri{db=?META_DB, table= ?TABLE_ID, row= ?TABLE_ID}).
-define(ROW, #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"row">>}).
-define(COLOUMN,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"coloumn">>}).
-define(LITERAL,  #row_uri{db=?META_DB, table=?TABLE_ID, row= ?COLOUMN_TYPE_ID}).
-define(VIEW,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"view">>}).
-define(HIERARCHY,  #row_uri{db=?META_DB, table=?TABLE_ID, row= <<"hierarchy">>).

% Property Types
-define(COLOUMN_TYPE_STRING, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"string">>}).
-define(COLOUMN_TYPE_BOOLEAN, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"boolean">>}).
-define(COLOUMN_TYPE_NUMBER, #row_uri{db=?META_DB, table=?LITERAL_ID, row= <<"number">>}).

-define(ARITY_ONE, <<"one">>).
-define(ARITY_MANY, <<"many">>).

% Core Coloumns
-define(URI_ID, <<"uri">>).
-define(URI, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= ?URI_ID}).
-define(COLOUMN_LABEL, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"label">>}).
-define(COLOUMN_LEGALCOLOUMNS, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"legal_coloumns">>}).
-define(COLOUMN_PARENTS, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"parents">>}).
-define(COLOUMN_RANGE, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"range">>}).
-define(COLOUMN_ARITY, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"arity">>}).
-define(COLOUMN_OPTIONAL, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"optional">>}).
-define(COLOUMN_INVERSE, #row_uri{db=?META_DB, table=?COLOUMN_ID, row= <<"inverse">>}).

% Core Records
-record(row, {uri, label, coloumns=[]}).
-record(table, {uri, label, coloumns=[], parents=[?ROW], legal_coloumns=[]}).
-record(coloumn, {uri, label, coloumns=[], range=[?ROW], arity=?ARITY_ONE, inverse, optional=false}).

% Database records
-record(db_rows, {row_id, label, coloumns=[]}).
-record(db_rows2table, {row_id, table_id}).
-record(db_table_includes, {table_id, included_table_id}).