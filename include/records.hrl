
-record(generic, {uri, properties}).

-record(item, {uri, label, types=[], properties=[]}).
-record(type, {uri, label, types=[<<"type">>], properties=[], parents=[<<"item">>], legal_properties=[]}).
-record(property, {uri, label, types=[<<"property">>], properties=[], ranges=[<<"item">>], arity=one, inverse}).
