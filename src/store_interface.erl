-module(store_interface).

-export([write/2, read/2, validate_item/2]).

-include("../include/records.hrl").

write(Item, Type) ->
  case validate_item(Item, Type) of
    {true, FinalItem} -> store:write_all([FinalItem]);
    {false, _} -> {error, requires_properties}
  end.

read(Item, Type) ->
  {ok, success}.

validate_item(Item=#item{uri=URI}, TypeURI) ->
  MergedItem = case store:read_item(URI) of
    undefined -> Item;
    OldItem -> merge_items(OldItem, Item, TypeURI)
  end,
  {validate_type_requirements(MergedItem), MergedItem}.

merge_items(OldItem, NewItem, NewType) ->
  #item{label=OldLabel, types=OldTypes, properties=OldProperties} = OldItem,
  #item{label=NewLabel, types=NewTypes, properties=NewProperties} = NewItem,
  MergedLabel = case NewLabel of
    undefined -> OldLabel;
    _ -> NewLabel
  end,
  MergedTypes = case NewTypes of
    [] -> [NewType|OldTypes];
    _Any -> case lists:member(NewType, NewTypes) of
      true -> NewTypes;
      false -> [NewType|NewTypes]
     end
  end,
  MergedProperties = merge_properties(OldProperties, NewProperties),
  NewItem#item{label=MergedLabel, types=MergedTypes, properties=MergedProperties}.

merge_properties(OldProperties, NewProperties) ->
  LeftOutProps = [Prop || Prop <- OldProperties, lists:member(Prop, NewProperties) == false],
  LeftOutProps ++ NewProperties.

validate_type_requirements(#item{label=Label, properties=Properties, types=Types}) ->
  case Label of
    undefined -> false;
    _ ->
      PropURIs = [URI || {URI, _} <- Properties],
      ParentTypeURIs = Types ++ lists:flatten([store:read_parents(Type) || Type <- Types]),
      ParentTypes = [store:read_type(Each) || Each <- ParentTypeURIs],
      RequiredProps = lists:flatten([Props || #type{legal_properties=Props} <- ParentTypes]),
      io:format("~p~n", [RequiredProps]),
      lists:all(fun(Prop) -> lists:member(Prop, PropURIs) end, RequiredProps)
  end.