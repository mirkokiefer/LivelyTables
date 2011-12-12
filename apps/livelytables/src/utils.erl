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
  row_uri2table_uri/1, row_coloumn/2,
  row2coloumnlist/1, coloumnlist2row/1,
  row2table/1, table2row/1, row2coloumn/1, coloumn2row/1,
  json/1, json2row/1, json2table/1, json2coloumn/1]).

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

row_coloumn(PropURI, #row{coloumns=Coloumns}) ->
  case lists:keyfind(PropURI, 1, Coloumns) of
    false -> undefined;
    {_, Value} -> Value
  end.

row2coloumnlist(#row{uri=URI, label=Label, coloumns=Coloumns}) ->
  [
    {?URI, URI},
    {?COLOUMN_LABEL, Label}
  ] ++ Coloumns.

coloumnlist2row([{_, URI}, {_, Label}|Rest]) ->
  #row{uri=URI, label=Label, coloumns=Rest}.

row2table(undefined) -> undefined;

row2table(#row{uri=URI, label=Label, coloumns=Coloumns}) ->
  row2table(Coloumns, #table{uri=URI, label=Label}).

row2table([{ColoumnURI, Value}|Rest], Table=#table{coloumns=Coloumns}) ->
  NewTable = case ColoumnURI of
    ?COLOUMN_PARENTS -> Table#table{parents=Value};
    ?COLOUMN_LEGALCOLOUMNS -> Table#table{legal_coloumns=Value};
    _ -> Table#table{coloumns=[{ColoumnURI, Value}|Coloumns]}
  end,
  row2table(Rest, NewTable);
row2table([], Table) -> Table.

table2row(undefined) -> undefined;

table2row(Table) ->
  #row{uri=Table#table.uri, label=Table#table.label, coloumns=[
    {?COLOUMN_PARENTS, Table#table.parents},
    {?COLOUMN_LEGALCOLOUMNS, Table#table.legal_coloumns}
  ] ++ Table#table.coloumns}.

row2coloumn(undefined) -> undefined;

row2coloumn(#row{uri=URI, label=Label, coloumns=Coloumns}) ->
  row2coloumn(Coloumns, #coloumn{uri=URI, label=Label}).

row2coloumn([{ColoumnURI, Value}|Rest], Coloumn=#coloumn{coloumns=Coloumns}) ->
  NewColoumn = case ColoumnURI of
    ?COLOUMN_RANGE -> Coloumn#coloumn{range=Value};
    ?COLOUMN_ARITY -> Coloumn#coloumn{arity=Value};
    ?COLOUMN_INVERSE -> Coloumn#coloumn{inverse=Value};
    ?COLOUMN_OPTIONAL -> Coloumn#coloumn{optional=Value};
    _ -> Coloumn#coloumn{coloumns=[{ColoumnURI, Value}|Coloumns]}
  end,
  row2coloumn(Rest, NewColoumn);
row2coloumn([], Coloumn) -> Coloumn.

coloumn2row(undefined) -> undefined;

coloumn2row(Coloumn) ->
  Row=#row{uri=Coloumn#coloumn.uri, label=Coloumn#coloumn.label,
    coloumns=[
      {?COLOUMN_RANGE, Coloumn#coloumn.range},
      {?COLOUMN_ARITY, Coloumn#coloumn.arity},
      {?COLOUMN_OPTIONAL, Coloumn#coloumn.optional}
    ] ++ Coloumn#coloumn.coloumns
  },
  #row{coloumns=Coloumns}=Row,
  case Coloumn#coloumn.inverse of
    undefined -> Row;
    Value -> Row#row{coloumns=[{?COLOUMN_INVERSE, Value}|Coloumns]}
  end.

json(Element) -> mochijson2:encode(struct(Element)).

struct([First|Rest]) -> [struct(First)|struct(Rest)];

struct(Row=#row{uri=URI, coloumns=Coloumns}) ->
  URIColoumns = case URI of
    undefined -> [];
    _ -> [{?URI_ID, record2uri(URI)}]
  end,
  {struct, URIColoumns ++ [
    {record2uri(?COLOUMN_LABEL), Row#row.label}
  ] ++ [{record2uri(Coloumn), struct(Value)} || {Coloumn, Value} <- Coloumns]};

struct(Table=#table{}) -> struct(table2row(Table));

struct(Coloumn=#coloumn{}) -> struct(coloumn2row(Coloumn));

struct(URI=#row_uri{}) -> record2uri(URI);

struct(Any) -> Any.

json2row({struct, Elements}) ->
  parse_row_elements(Elements, #row{}).

json2table({struct, Elements}) ->
  row2table(parse_row_elements(Elements, #row{})).

json2coloumn({struct, Elements}) ->
  row2coloumn(parse_row_elements(Elements, #row{})).

parse_row_elements([{Key, Value}|Rest], Row=#row{coloumns=Coloumns}) ->
  NewRow = case Key of
    ?URI -> Row#row{uri=Value};
    ?COLOUMN_LABEL -> Row#row{label=Value};
    _ -> Row#row{coloumns=[{Key, parse_coloumn_value(Value)}|Coloumns]}
  end,
  parse_row_elements(Rest, NewRow);
parse_row_elements([], Row) -> Row.

parse_coloumn_value([]) -> [];
parse_coloumn_value([First|Rest]) -> [parse_coloumn_value(First)|parse_coloumn_value(Rest)];
parse_coloumn_value({struct, Elements}) -> json2row({struct, Elements});
parse_coloumn_value(Literal) -> Literal.