%%%------------------------------------------------------------------- 
%%% @author Mirko Kiefer <mail@mirkokiefer.com>
%%% [http://www.mirkokiefer.com]
%%% @copyright 2011 Mirko Kiefer
%%% @doc HTTP server
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([time_seconds/1, write_file/2, read_file/1, log/1, encode/1,
  uri2record/1, record2uri/1,
  set/1, filter_element/2, is_joint/2, is_disjoint/2, is_subset/2,
  row_uri2table_uri/1, row_column/2,
  row2columnlist/1, columnlist2row/1,
  row2table/1, table2row/1, row2column/1, column2row/1,
  json/1, json2row/1, json2table/1, json2column/1]).

-include("../include/records.hrl").

time_seconds(Fun) ->
  StartT = now(),
  Result = Fun(),
  EndT = now(),
  Time = timer:now_diff(EndT, StartT)/1000000,
  {{time, Time}, {result, Result}}.

write_file(File, List) ->
  {ok, S} = file:open(File, write),
  lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, List), file:close(S).

read_file(File) -> file:consult(File).

log(Message) -> io:format("~p~n", [Message]).

encode(String) ->
  unicode:characters_to_binary(io_lib:format("~ts", [String])).

uri2record(URIString) ->
  [Domain, DB, Table, Row] = [list_to_binary(Each) || Each <- string:tokens(URIString, "/")],
  #row_uri{domain=Domain, db=DB, table=Table, row=Row}.

record2uri(#row_uri{domain=Domain, db=DB, table=Table, row=Row}) ->
  URIComponents = [binary_to_list(Each) || Each <- [Domain, DB, Table, Row]],
  list_to_binary(string:join(URIComponents, "/")).

set(List) ->
  sets:to_list(sets:from_list(List)).

filter_element([First|Rest], ValidElements) ->
  case lists:member(First, ValidElements) of
    true -> First;
    false -> filter_element(Rest, ValidElements)
  end;

filter_element([], _) -> undefined.

is_joint(List1, List2) -> is_disjoint(List1, List2) == false.

is_disjoint(List1, List2) ->
  sets:is_disjoint(sets:from_list(List1), sets:from_list(List2)).

is_subset(Subset, List) ->
  sets:is_subset(sets:from_list(Subset), sets:from_list(List)).

row_uri2table_uri(#row_uri{domain=Domain, db=DB, table=Table}) ->
  #row_uri{domain=Domain, db=DB, table=?TABLE_ID, row=Table}.

row_column(PropURI, #row{columns=Columns}) ->
  case lists:keyfind(PropURI, 1, Columns) of
    false -> undefined;
    {_, Value} -> Value
  end.

row2columnlist(#row{uri=URI, label=Label, columns=Columns}) ->
  [
    {?URI, URI},
    {?COLUMN_LABEL, Label}
  ] ++ Columns.

columnlist2row([{_, URI}, {_, Label}|Rest]) ->
  #row{uri=URI, label=Label, columns=Rest}.

row2table(undefined) -> undefined;

row2table(#row{uri=URI, label=Label, columns=Columns}) ->
  row2table(Columns, #table{uri=URI, label=Label}).

row2table([{ColumnURI, Value}|Rest], Table=#table{columns=Columns}) ->
  NewTable = case ColumnURI of
    ?COLUMN_PARENTS -> Table#table{parents=Value};
    ?COLUMN_LEGALCOLUMNS -> Table#table{legal_columns=Value};
    _ -> Table#table{columns=[{ColumnURI, Value}|Columns]}
  end,
  row2table(Rest, NewTable);
row2table([], Table) -> Table.

table2row(undefined) -> undefined;

table2row(Table) ->
  #row{uri=Table#table.uri, label=Table#table.label, columns=[
    {?COLUMN_PARENTS, Table#table.parents},
    {?COLUMN_LEGALCOLUMNS, Table#table.legal_columns}
  ] ++ Table#table.columns}.

row2column(undefined) -> undefined;

row2column(#row{uri=URI, label=Label, columns=Columns}) ->
  row2column(Columns, #column{uri=URI, label=Label}).

row2column([{ColumnURI, Value}|Rest], Column=#column{columns=Columns}) ->
  NewColumn = case ColumnURI of
    ?COLUMN_RANGE -> Column#column{range=Value};
    ?COLUMN_ARITY -> Column#column{arity=Value};
    ?COLUMN_INVERSE -> Column#column{inverse=Value};
    ?COLUMN_OPTIONAL -> Column#column{optional=Value};
    _ -> Column#column{columns=[{ColumnURI, Value}|Columns]}
  end,
  row2column(Rest, NewColumn);
row2column([], Column) -> Column.

column2row(undefined) -> undefined;

column2row(Column) ->
  Row=#row{uri=Column#column.uri, label=Column#column.label,
    columns=[
      {?COLUMN_RANGE, Column#column.range},
      {?COLUMN_ARITY, Column#column.arity},
      {?COLUMN_OPTIONAL, Column#column.optional}
    ] ++ Column#column.columns
  },
  #row{columns=Columns}=Row,
  case Column#column.inverse of
    undefined -> Row;
    Value -> Row#row{columns=[{?COLUMN_INVERSE, Value}|Columns]}
  end.

json(Element) -> mochijson2:encode(struct(Element)).

struct([First|Rest]) -> [struct(First)|struct(Rest)];

struct(Row=#row{uri=URI, columns=Columns}) ->
  URIColumns = case URI of
    undefined -> [];
    _ -> [{?URI_ID, record2uri(URI)}]
  end,
  {struct, URIColumns ++ [
    {record2uri(?COLUMN_LABEL), Row#row.label}
  ] ++ [{record2uri(Column), struct(Value)} || {Column, Value} <- Columns]};

struct(Table=#table{}) -> struct(table2row(Table));

struct(Column=#column{}) -> struct(column2row(Column));

struct(URI=#row_uri{}) -> record2uri(URI);

struct(Any) -> Any.

json2row({struct, Elements}) ->
  parse_row_elements(Elements, #row{}).

json2table({struct, Elements}) ->
  row2table(parse_row_elements(Elements, #row{})).

json2column({struct, Elements}) ->
  row2column(parse_row_elements(Elements, #row{})).

parse_row_elements([{Key, Value}|Rest], Row=#row{columns=Columns}) ->
  NewRow = case Key of
    ?URI -> Row#row{uri=Value};
    ?COLUMN_LABEL -> Row#row{label=Value};
    _ -> Row#row{columns=[{Key, parse_column_value(Value)}|Columns]}
  end,
  parse_row_elements(Rest, NewRow);
parse_row_elements([], Row) -> Row.

parse_column_value([]) -> [];
parse_column_value([First|Rest]) -> [parse_column_value(First)|parse_column_value(Rest)];
parse_column_value({struct, Elements}) -> json2row({struct, Elements});
parse_column_value(Literal) -> Literal.
