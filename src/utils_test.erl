-module(utils_test).

-include("../include/records.hrl").

-export([test/0]).

test() ->
  test_generic2item(),
  test_item2generic(),
  test_generic2type(),
  test_type2generic(),
  test_generic2property(),
  test_property2generic().

test_generic2item() ->
  true = sample_item() == utils:generic2item(sample_generic_item()),
  {ok, success}.

test_item2generic() ->
  true = sample_generic_item() == utils:item2generic(sample_item()),
  {ok, success}.

test_generic2type() ->
  true = sample_type() == utils:generic2type(sample_generic_type()),
  {ok, success}.

test_type2generic() ->
  true = sample_generic_type() == utils:type2generic(sample_type()),
  {ok, success}.

test_generic2property() ->
  true = sample_property() == utils:generic2property(sample_generic_property()),
  {ok, success}.

test_property2generic() ->
  true = sample_generic_property() == utils:property2generic(sample_property()),
  {ok, success}.

sample_generic_item() ->
  #generic{uri= <<"bob">>, properties=[
    {<<"label">>, <<"Bob">>},
    {<<"types">>, [<<"person">>]},
    {<<"owns">>, <<"google">>}
  ]}.

sample_item() ->
  #item{uri= <<"bob">>, label= <<"Bob">>, types=[<<"person">>], properties=[
    {<<"owns">>, <<"google">>}
  ]}.

sample_generic_type() ->
  #generic{uri= <<"person">>, properties=[
    {<<"label">>, <<"Person">>},
    {<<"types">>, [<<"type">>]},
    {<<"parents">>, [<<"item">>]},
    {<<"legal_properties">>, [<<"owns">>]},
    {<<"description">>, <<"I'm a Person Type">>}
  ]}.

sample_type() ->
  #type{uri= <<"person">>, label= <<"Person">>, types=[<<"type">>], parents=[<<"item">>],
    properties=[
      {<<"description">>, <<"I'm a Person Type">>}
    ], legal_properties=[<<"owns">>]
  }.

sample_generic_property() ->
  #generic{uri= <<"owns">>, properties=[
    {<<"label">>, <<"owns">>},
    {<<"types">>, [<<"property">>]},
    {<<"ranges">>, [<<"item">>]},
    {<<"arity">>, <<"many">>},
    {<<"inverse">>, undefined}
  ]}.

sample_property() ->
  #property{uri= <<"owns">>, label= <<"owns">>, types=[<<"property">>],
    ranges=[<<"item">>], arity= <<"many">>}.