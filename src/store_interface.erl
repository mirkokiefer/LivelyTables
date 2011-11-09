-module(store_interface).

-export([transaction/1, write_item/1, write_item/2, write_type/1, write_property/1,
  read_item/2, read_type/1, read_property/1, read_items_of_type/1,
  validate/1]).

-include("../include/records.hrl").

transaction(Fun) ->
  store:transaction(Fun).

read_item(ItemURI, TypeURI) ->
  Item = #item{properties=Properties} = store:read_item(ItemURI),
  ParentTypeURIs = [TypeURI|store:read_parents(TypeURI)],
  ParentTypes = [store:read_type(URI) || URI <- ParentTypeURIs],
  LegalProps = lists:flatten([Legal || #type{legal_properties=Legal} <- ParentTypes]),
  FilteredProps = [Property || Property={URI,_} <- Properties, lists:member(URI, LegalProps)],
  Item#item{properties=FilteredProps}.

read_type(TypeURI) -> store:read_type(TypeURI).

read_property(PropertyURI) -> store:read_property(PropertyURI).

read_items_of_type(TypeURI) -> store:read_items_of_type(TypeURI).

write_item(Item) -> write_item(Item, ?ITEM).

write_item(Item=#item{uri=URI}, Type) ->
  OldItem = store:read_item(URI),
  write(Item, OldItem, Type, fun(FinalItem) -> FinalItem end).

write_type(Type=#type{uri=URI}) ->
  OldItem = store:read_type_item(URI),
  write(utils:type2item(Type), OldItem, ?TYPE, fun(FinalItem) -> utils:item2type(FinalItem) end).

write_property(Property=#property{uri=URI}) ->
  OldItem = store:read_property_item(URI),
  write(utils:property2item(Property), OldItem, ?PROPERTY, fun(FinalItem) -> utils:item2property(FinalItem) end).

write(Item, OldItem, Type, ConversionFun) ->
  MergedItem = merge(Item, OldItem, Type),
  case validate(MergedItem) of
    {true, _} -> store:write_all([ConversionFun(MergedItem)]);
    {false, Errors} -> {error, Errors}
  end.

merge(Item=#item{types=Types}, undefined, TypeURI) ->
  case lists:member(TypeURI, Types) of
      true -> Item;
      false -> Item#item{types=[TypeURI|Types]}
  end;

merge(Item, OldItem, TypeURI) -> merge_items(Item, OldItem, TypeURI).

merge_items(NewItem, OldItem, NewType) ->
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
  #type{legal_properties=LegalProperties} = store:read_type(NewType),
  MergedProperties = merge_properties(OldProperties, NewProperties, LegalProperties),
  NewItem#item{label=MergedLabel, types=MergedTypes, properties=MergedProperties}.

merge_properties(OldProperties, NewProperties, LegalProperties) ->
  NewPropertyURIs = [URI || {URI, _} <- NewProperties],
  LeftOutProps = [Prop || Prop={URI,_} <- OldProperties,
    lists:member(URI, NewPropertyURIs) == false,
    lists:member(URI, LegalProperties) == false
  ],
  LeftOutProps ++ NewProperties.

validate(Type = #type{}) -> validate_item(utils:type2item(Type));
validate(Property = #property{}) -> validate_item(utils:property2item(Property));
validate(Item = #item{}) -> validate_item(Item).

validate_item(Item) ->
  {ValidType, TypeErrors} = validate_type_requirements(Item),
  {ValidProperties, PropertyErrors} = validate_properties(Item),
  {ValidType and ValidProperties, TypeErrors++PropertyErrors}.

validate_type_requirements(Item=#item{types=Types}) ->
  ParentTypeURIs = utils:set(Types ++ lists:flatten([store:read_parents(Type) || Type <- Types])),
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
    false -> case store:read_property(LegalProperty) of
      #property{optional=false} -> {false, legal_property_missing(LegalProperty)};
      #property{optional=true} -> {true, []}
    end
  end.

validate_properties(#item{types=Types, properties=Properties}) ->
  {Valid, Errors} = case Types of
    [] -> {false, legal_property_missing(?PROPERTY_TYPES)};
    _ -> {true, []}
  end,
  {ValuesValid, ValuesErrors} = validate_property_values(Properties),
  {Valid and ValuesValid, Errors ++ ValuesErrors}.

validate_property_values(Properties) -> validate_property_values(Properties, {true, []}).

validate_property_values([{PropertyURI, Value}|Rest], {Valid, Errors}) ->
  {ValidProperty, PropertyErrors} = case store:read_property(PropertyURI) of
    undefined -> {false, property_not_exists(PropertyURI, Value)};
    #property{range=Range, arity=Arity} -> 
      Result = validate_property_range(Range, Arity, Value),
      case Result of
        {true, _} -> {true, []};
        {false, RangeErrors} -> {false, invalid_property_value(PropertyURI, Range, RangeErrors)}
      end
  end,
  validate_property_values(Rest, {Valid and ValidProperty, PropertyErrors ++ Errors});
validate_property_values([], Result) -> Result.

validate_property_range(_Range, ?ARITY_MANY, []) -> {true, []};
validate_property_range(Range, ?ARITY_MANY, Values=[_|_]) ->
  Results = [validate_property_range(Range, ?ARITY_ONE, Value) || Value <- Values],
  sum_result(Results);
validate_property_range(_Range, ?ARITY_MANY, _Value) -> {false, arity_error()};
validate_property_range(?PROPERTY_TYPE_STRING, ?ARITY_ONE, Value) -> {is_bitstring(Value), []};
validate_property_range(?PROPERTY_TYPE_NUMBER, ?ARITY_ONE, Value) -> {is_number(Value), []};
validate_property_range(?PROPERTY_TYPE_BOOLEAN, ?ARITY_ONE, Value) -> {is_boolean(Value), []};
validate_property_range(_, ?ARITY_ONE, ?PROPERTY_TYPE_NUMBER) -> {true, []};
validate_property_range(_, ?ARITY_ONE, ?PROPERTY_TYPE_STRING) -> {true, []};
validate_property_range(_, ?ARITY_ONE, ?PROPERTY_TYPE_BOOLEAN) -> {true, []};

validate_property_range(Range, ?ARITY_ONE, Value=#type{}) ->
  validate_property_range(Range, ?ARITY_ONE, utils:type2item(Value));

validate_property_range(Range, ?ARITY_ONE, Value=#property{}) ->
  validate_property_range(Range, ?ARITY_ONE, utils:property2item(Value));

validate_property_range(Range, ?ARITY_ONE, Value=#item{types=Types}) ->
  case validate_item(Value) of
    {true, _} ->
      Parents = Types ++ lists:flatten([store:read_parents(Type) || Type <- Types]),
      {lists:member(Range, Parents), []};
    {false, Errors} -> {false, Errors}
  end;
validate_property_range(Range, ?ARITY_ONE, Value) ->
  {lists:member(Range, store:read_types_of_item(Value)), []}.

%Merges Error results
sum_result(Result) -> sum_result(Result, {true, []}).
sum_result([{Valid, Errors}|Rest], {SumValid, SumErrors}) ->
  sum_result(Rest, {SumValid and Valid, Errors ++ SumErrors});
sum_result([], SumResult) -> SumResult.

%Error messages
legal_property_missing(LegalProperty) -> [[{message, <<"Property missing on item">>}, {value, LegalProperty}]].
property_not_exists(Property, Value) ->
  [[{message, <<"Property does not exist">>}, {property, Property}, {value, Value}]].
invalid_property_value(Property, Range, []) ->
  [[{message, <<"Invalid property value">>}, {range, Range}, {property,Property}]];
invalid_property_value(Property, Range, Details) ->
  [[{message, <<"Invalid property value">>}, {range, Range}, {property,Property},
    {details, Details}]].
arity_error() -> [[{message, <<"Arity is 'many' but only single value">>}]].