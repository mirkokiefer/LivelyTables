
% Property Types
-define(PROPERTY_TYPE_STRING, <<"string">>).
-define(PROPERTY_TYPE_BOOLEAN, <<"boolean">>).
-define(PROPERTY_TYPE_NUMBER, <<"number">>).

-define(ARITY_ONE, <<"one">>).
-define(ARITY_MANY, <<"many">>).

% Core Types
-define(ITEM, <<"item">>).
-define(TYPE, <<"type">>).
-define(PROPERTY, <<"property">>).
-define(VIEW, <<"view">>).
-define(HIERARCHY, <<"hierarchy">>).

% Core Properties
-define(URI, <<"uri">>).
-define(PROPERTY_LABEL, <<"label">>).
-define(PROPERTY_TYPES, <<"types">>).
-define(PROPERTY_LEGALPROPERTIES, <<"legal_properties">>).
-define(PROPERTY_PARENTS, <<"parents">>).
-define(PROPERTY_RANGE, <<"range">>).
-define(PROPERTY_ARITY, <<"arity">>).
-define(PROPERTY_OPTIONAL, <<"optional">>).
-define(PROPERTY_INVERSE, <<"inverse">>).

% Set Types
-define(SET, <<"set">>).
-define(SET_OPERATION, <<"set_operation">>).
-define(INTERSECTION, <<"intersection">>).
-define(UNION, <<"union">>).
-define(FILTER, <<"filter">>).
-define(PROPERTY_VALUE_FILTER, <<"property_value_filter">>).
-define(PROPERTY_EXISTENCE_FILTER, <<"property_existence_filter">>).
-define(VALUE_CONDITION, <<"value_condition">>).
-define(VALUE_CONDITION_EXACT, <<"value_condition_exact">>).

% Set Properties
-define(PROPERTY_SETS, <<"sets">>).
-define(PROPERTY_ON_PROPERTY, <<"on_property">>).
-define(PROPERTY_VALUE_CONDITION, <<"value_condition">>).
-define(PROPERTY_VALUE, <<"value">>).

-record(item, {uri, label, types=[], properties=[]}).
-record(type, {uri, label, types=[?TYPE], properties=[], parents=[?ITEM], legal_properties=[]}).
-record(property, {uri, label, types=[?PROPERTY], properties=[], range=[?ITEM], arity=?ARITY_ONE,
  inverse, optional=false}).
