-module(git).
-export([transaction/1, write/1, read_item/1, read_type/1, read_property/1]).
-include("../include/records.hrl").

write(Item=#item{uri=URI}) -> write(Item, item_path(URI));
write(Type=#type{uri=URI}) -> write(utils:type2item(Type), type_path(URI));
write(Property=#property{uri=URI}) -> write(utils:property2item(Property), property_path(URI)).

write(Item, Path) ->
  utils:write_file(Path, utils:item2propertylist(Item)).

read_item(URI) ->
  read(item_path(URI)).

read_type(URI) ->
  utils:item2type(read(type_path(URI))).

read_property(URI) ->
  utils:item2property(read(property_path(URI))).

read(Path) ->
  {ok, PropertyList} = utils:read_file(Path),
  utils:propertylist2item(PropertyList).

transaction(Fun) ->
  go_to_git(),
  Value = Fun(),
  commit(),
  go_to_src(),
  Value.


commit() ->
  os:cmd("git add -A"),
  os:cmd("git commit -m \"write items\"").


% utility functions
go_to_git() -> file:set_cwd("../git_store").
go_to_src() -> file:set_cwd("../src").

item_path(URI) -> "items/" ++ bitstring_to_list(URI) ++ ".txt".
type_path(URI) -> "types/" ++ item_path(URI).
property_path(URI) -> "properties/" ++ item_path(URI).