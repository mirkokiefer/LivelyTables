
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
-define(ITEM_LIST, <<"item_list">>).
-define(PROJECT_SET, <<"project_set">>).
-define(SET_OPERATION, <<"set_operation">>).
-define(INTERSECTION, <<"intersection">>).
-define(UNION, <<"union">>).

-define(TRANSFORM_SET, <<"set_transform">>).
-define(TRANSFORM_ITEMS_TO_VALUES, <<"transform_items_to_values">>).
-define(TRANSFORM_ITEMS_TO_PROPERTIES, <<"transform_items_to_properties">>).
-define(TRANSFORM_PROPERTIES_TO_ITEMS, <<"transform_properties_to_items">>).
-define(TRANSFORM_TYPES_TO_ITEMS, <<"transform_types_to_items">>).

-define(FILTER, <<"filter">>).

-define(CONDITION, <<"condition">>).
-define(PROPERTY_EXISTS_CONDITION, <<"property_exists_condition">>).
-define(VALUE_CONDITION, <<"value_condition">>).
-define(VALUE_CONDITION_EQUALS, <<"value_condition_equals">>).

% Set Properties
-define(PROPERTY_ITEMS, <<"property_items">>).
-define(PROPERTY_SET, <<"property_set">>).
-define(PROPERTY_SETS, <<"property_sets">>).
-define(PROPERTY_PROPERTY_SET, <<"property_property_set">>).
-define(PROPERTY_CONDITIONS, <<"property_conditions">>).
-define(PROPERTY_VALUE, <<"property_value">>).

-record(item, {uri, label, types=[], properties=[]}).
-record(type, {uri, label, types=[?TYPE], properties=[], parents=[?ITEM], legal_properties=[]}).
-record(property, {uri, label, types=[?PROPERTY], properties=[], range=[?ITEM], arity=?ARITY_ONE,
  inverse, optional=false}).

% Set records
-record(union, {sets}).
-record(intersection, {sets}).
-record(filter, {set, conditions}).
-record(items2values, {items, properties}).
-record(items2properties, {items}).
-record(properties2items, {properties}).
-record(types2items, {types}).

-record(value_equals, {properties, value}).
-record(property_exists, {properties}).
