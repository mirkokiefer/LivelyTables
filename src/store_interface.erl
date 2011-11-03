-module(store_interface).

-export([write/2, read_item/2, read_type/1, read_property/1, validate_item/2]).

-include("../include/records.hrl").

read_item(ItemURI, TypeURI) ->
  Item = #item{properties=Properties} = store:read_item(ItemURI),
  ParentTypeURIs = [TypeURI|store:read_parents(TypeURI)],
  ParentTypes = [store:read_type(URI) || URI <- ParentTypeURIs],
  LegalProps = lists:flatten([Legal || #type{legal_properties=Legal} <- ParentTypes]),
  FilteredProps = [Property || Property={URI,_} <- Properties, lists:member(URI, LegalProps)],
  Item#item{properties=FilteredProps}.

read_type(TypeURI) -> store:read_type(TypeURI).

read_property(PropertyURI) -> store:read_property(PropertyURI).

write(Item=#item{}, Type) ->
  write(Item, Type, fun(FinalItem) -> FinalItem end);

write(Type=#type{}, Type) ->
  write(utils:type2item(Type), Type, fun(FinalItem) -> utils:item2type(FinalItem) end);

write(Property=#property{}, Type) ->
  write(utils:property2item(Property), Type, fun(FinalItem) -> utils:item2property(FinalItem) end).

write(Item, Type, ConversionFun) ->
  case validate_item(Item, Type) of
    {true, FinalItem, _} -> store:write_all([ConversionFun(FinalItem)]);
    {false, _, Errors} -> {error, Errors}
  end.

validate_item(Item=#item{uri=URI}, TypeURI) ->
  MergedItem = case store:read_item(URI) of
    undefined -> Item;
    OldItem -> merge_items(OldItem, Item, TypeURI)
  end,
  {ValidType, TypeErrors} = validate_type_requirements(MergedItem),
  {ValidProperties, PropertyErrors} = validate_properties(MergedItem),
  {ValidType and ValidProperties, MergedItem, TypeErrors++PropertyErrors}.

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
  NewPropertyURIs = [URI || {URI, _} <- NewProperties],
  LeftOutProps = [Prop || Prop={URI,_} <- OldProperties, lists:member(URI, NewPropertyURIs) == false],
  LeftOutProps ++ NewProperties.

validate_type_requirements(Item=#item{types=Types}) ->
  ParentTypeURIs = Types ++ lists:flatten([store:read_parents(Type) || Type <- Types]),
  ParentTypes = [store:read_type(Each) || Each <- ParentTypeURIs],
  Results = [validate_legal_properties(LegalProps, Item) || #type{legal_properties=LegalProps} <- ParentTypes],
  sum_result(Results).

validate_legal_properties(Properties, Item) -> validate_legal_properties(Properties, Item, {true, []}).

validate_legal_properties([First|Rest], Item, {SumValid, SumErrors}) ->
  {Valid, Errors} = validate_legal_property(First, Item),
  validate_legal_properties(Rest, Item, {SumValid and Valid, Errors++SumErrors});
validate_legal_properties([], _, Result) -> Result.

validate_legal_property(?PROPERTY_LABEL, #item{}) -> {true, []};

validate_legal_property(?PROPERTY_TYPES, #item{}) -> {true, []};

validate_legal_property(LegalProperty, #item{properties=Properties}) ->
  PropertyURIs = [PropertyURI || {PropertyURI, _} <- Properties],
  case lists:member(LegalProperty, PropertyURIs) of
    true -> {true, []};
    false -> {false, [{missing, LegalProperty}]}
  end.

validate_properties(#item{label=Label, types=Types, properties=Properties}) ->
  {Valid, Errors} = case {Label, Types} of
    {undefined, _} -> {false, [{missing, ?PROPERTY_LABEL}]};
    {_, []} -> {false, [{missing, ?PROPERTY_TYPES}]};
    _ -> {true, []}
  end,
  {ValuesValid, ValuesErrors} = validate_property_values(Properties),
  {Valid and ValuesValid, Errors ++ ValuesErrors}.

validate_property_values(Properties) -> validate_property_values(Properties, {true, []}).

validate_property_values([{PropertyURI, Value}|Rest], {Valid, Errors}) ->
  #property{ranges=Ranges, arity=Arity} = store:read_property(PropertyURI),
  Result = lists:any(fun(Range) -> validate_property_range(Range, Arity, Value) end, Ranges),
  {ValidProperty, PropertyErrors} = case Result of
    true -> {true, []};
    false -> {false, [{invalid_property, {ranges, Ranges}, {property, PropertyURI}, {value, Value}}]}
  end,
  validate_property_values(Rest, {Valid and ValidProperty, PropertyErrors ++ Errors});
validate_property_values([], Result) -> Result.

validate_property_range(Range, ?ARITY_MANY, Values) ->
  lists:all(fun(Value) -> validate_property_range(Range, ?ARITY_ONE, Value) end, Values);

validate_property_range(?PROPERTY_TYPE_STRING, ?ARITY_ONE, Value) -> is_bitstring(Value);

validate_property_range(?PROPERTY_TYPE_NUMBER, ?ARITY_ONE, Value) -> is_number(Value);

validate_property_range(?PROPERTY_TYPE_BOOLEAN, ?ARITY_ONE, Value) -> is_boolean(Value);

validate_property_range(Range, ?ARITY_ONE, Value) ->
  lists:member(Range, store:read_types_of_item(Value)).

sum_result(Result) -> sum_result(Result, {true, []}).
  
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.