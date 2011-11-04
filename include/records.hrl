
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
-define(PROPERTY_RANGES, <<"ranges">>).
-define(PROPERTY_ARITY, <<"arity">>).
-define(PROPERTY_OPTIONAL, <<"optional">>).
-define(PROPERTY_INVERSE, <<"inverse">>).

-record(item, {uri, label, types=[], properties=[]}).
-record(type, {uri, label, types=[?TYPE], properties=[], parents=[?ITEM], legal_properties=[]}).
-record(property, {uri, label, types=[?PROPERTY], properties=[], ranges=[?ITEM], arity=?ARITY_ONE,
  inverse, optional=false}).
