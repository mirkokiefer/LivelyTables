-module(store_interface).

-export([write/2, read/2, validate_item/2]).

-include("../include/records.hrl").

write(Item=#item{}, Type) ->
  write(Item, Type, fun(FinalItem) -> FinalItem end);

write(Type=#type{}, Type) ->
  write(utils:type2item(Type), Type, fun(FinalItem) -> utils:item2type(FinalItem) end);

write(Property=#property{}, Type) ->
  write(utils:property2item(Property), Type, fun(FinalItem) -> utils:item2property(FinalItem) end).

write(Item, Type, ConversionFun) ->
  case validate_item(Item, Type) of
    {true, FinalItem} -> store:write_all([ConversionFun(FinalItem)]);
    {false, _} -> {error, invalid_item}
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

validate_type_requirements(Item=#item{types=Types}) ->
  ParentTypeURIs = Types ++ lists:flatten([store:read_parents(Type) || Type <- Types]),
  ParentTypes = [store:read_type(Each) || Each <- ParentTypeURIs],
  lists:all(fun(#type{legal_properties=LegalProps}) ->
    validate_properties(LegalProps, Item) end, ParentTypes).

validate_properties([First|Rest], Item) ->
  validate_property(First, Item) and validate_properties(Rest, Item);
validate_properties([], _) -> true.

validate_property(<<"label">>, #item{label=Label}) ->
  Label =/= undefined;

validate_property(<<"types">>, #item{types=Types}) ->
  length(Types) > 0;

validate_property(LegalProperty, #item{properties=Properties}) ->
  PropertyURIs = [PropertyURI || {PropertyURI, _} <- Properties],
  lists:member(LegalProperty, PropertyURIs).