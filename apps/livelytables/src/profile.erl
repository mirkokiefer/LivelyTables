-module(profile).
-export([start_trace/0, stop_trace/0, analyse/0, parse/0, read_trace_data/1, read_trace_data/3, module_report/2]).

-include("../include/records.hrl").

-record(trace_data, {function, properties}).
  
start_trace() -> fprof:trace(start, "sapiento.trace").

stop_trace() -> fprof:trace(stop).

analyse() ->
  fprof:profile(file, "sapiento.trace"),
  fprof:analyse([{dest, "sapiento.txt"}, {cols, 120}]).

init_tables() ->
  reset_tables(),
  ets:new(trace_misc, [named_table, set, public]),
  ets:new(trace_data, [named_table, set, public, {keypos, 2}]),
  ets:new(trace_functions, [named_table, duplicate_bag, public]).

reset_tables() ->
  case ets:info(trace_data) of
    undefined -> no_tables;
    _ -> ets:delete(trace_data),
      ets:delete(trace_functions)
  end.
  
store_trace_data(TraceData=#trace_data{function={Module, Fun, Arity}}) ->
  ets:insert(trace_data, TraceData),
  ets:insert(trace_functions, {Module, {Fun, Arity}}).
  
store_totals(Totals) ->
  ets:insert(trace_misc, {totals, Totals}).

read_trace_data(Module) ->
  Funs = read_funs_of_module(Module),
  [read_trace_data(Module, Fun, Arity) || {_, Fun, Arity} <- Funs].

read_trace_data(Module, Function, Arity) ->
  [Result] = ets:lookup(trace_data, {Module, Function, Arity}),
  Result.
  
read_funs_of_module(Module) ->
  Result = ets:lookup(trace_functions, Module),
  [{Module, Fun, Arity} || {_, {Fun, Arity}} <- Result].

module_report({Command, Property}, Module) ->
  TraceData = read_trace_data(Module),
  FilteredData = [{Function, lookup(Property, Props)} || #trace_data{function=Function, properties=Props} <- TraceData],
  calc_report(Command, FilteredData).

calc_report(sort, Data) ->
  lists:sort(fun({_, Own1}, {_, Own2}) -> Own1 > Own2 end, Data);

calc_report(sum, [{_Fun, Value}|Rest]) ->
  Value + calc_report(sum, Rest);
calc_report(sum, []) -> 0.

parse() ->
  init_tables(),
  {ok, Data} = utils:read_file("sapiento.txt"),
  parse_data(Data).
  
parse_data([_Options, Totals, _|Functions]) ->
  store_totals(Totals),
  parse_functions(Functions).

parse_functions([First|Rest]) -> parse_function(First), parse_functions(Rest);
parse_functions([]) -> done.

parse_function({CallingFuns, {{Module, Fun, Arity}, Count, Acc, Own}, CalledFuns}) ->
  case lists:member(Module, profile_modules()) of
    true -> store_trace_data(#trace_data{function={Module, Fun, Arity}, properties=[
      {count, Count},
      {acc, Acc},
      {own, Own},
      {callers, CallingFuns},
      {calls, CalledFuns}
    ]});
    false -> ignore
  end;
parse_function(_Any) -> [].

profile_modules() -> [server, store, store_interface, utils].

lookup(Property, PropertyList) ->
  proplists:get_value(Property, PropertyList).
  