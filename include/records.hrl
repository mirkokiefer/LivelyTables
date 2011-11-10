
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
-define(PROJECT_SET, <<"project_set">>).
-define(SET_OPERATION, <<"set_operation">>).
-define(INTERSECTION, <<"intersection">>).
-define(UNION, <<"union">>).

-define(TRANSFORM_SET, <<"set_transform">>).
-define(TRANSFORM_VALUES_TO_ITEMS, <<"transform_values_to_items">>).
-define(TRANSFORM_PROPERTIES_TO_ITEMS, <<"transform_properties_to_items">>).
-define(TRANSFORM_PROPERTY_ITEMS_TO_ITEMS, <<"transform_property_items_to_items">>).

-define(FILTER, <<"filter">>).
-define(FILTER_TYPES, <<"filter_types">>).
-define(FILTER_TYPES_LIST, <<"filter_types_list">>).
-define(FILTER_TYPES_SET, <<"filter_types_set">>).
-define(FILTER_ITEMS, <<"filter_items">>).
-define(FILTER_ITEMS_LIST, <<"filter_items_list">>).
-define(FILTER_ITEMS_SET, <<"filter_items_set">>).

-define(FILTER_PROPERTY_EXISTENCE, <<"property_existence_filter">>).
-define(FILTER_PROPERTY_EXISTENCE_LIST, <<"property_existence_filter_list">>).
-define(FILTER_PROPERTY_EXISTENCE_SET, <<"property_existence_filter_set">>).

-define(FILTER_PROPERTY_VALUE, <<"property_value_filter">>).
-define(FILTER_PROPERTY_VALUE_LIST, <<"property_value_filter_list">>).
-define(FILTER_PROPERTY_VALUE_SET, <<"property_value_filter_set">>).

-define(VALUE_CONDITION, <<"value_condition">>).
-define(CONDITION_EQUAL, <<"value_condition_exact">>).

% Set Properties
-define(PROPERTY_SETS, <<"property_sets">>).
-define(PROPERTY_TYPE_LIST, <<"property_type_list">>).
-define(PROPERTY_ITEM_LIST, <<"property_item_list">>).
-define(PROPERTY_PROPERTY_LIST, <<"property_property_list">>).
-define(PROPERTY_TYPE_SET, <<"property_type_set">>).
-define(PROPERTY_ITEM_SET, <<"property_item_set">>).
-define(PROPERTY_PROPERTY_SET, <<"property_property_set">>).
-define(PROPERTY_SET, <<"property_set">>).
-define(PROPERTY_VALUE_CONDITION, <<"property_value_condition">>).
-define(PROPERTY_VALUE, <<"property_value">>).

-record(item, {uri, label, types=[], properties=[]}).
-record(type, {uri, label, types=[?TYPE], properties=[], parents=[?ITEM], legal_properties=[]}).
-record(property, {uri, label, types=[?PROPERTY], properties=[], range=[?ITEM], arity=?ARITY_ONE,
  inverse, optional=false}).
