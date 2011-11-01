
-record(item, {uri, label, types=[<<"thing">>], properties=[]}).
-record(type, {uri, label, types=[<<"type">>], properties=[], parents=[<<"thing">>], legal_properties=[]}).
-record(property, {uri, label, types=[<<"property">>], properties=[], ranges=[<<"thing">>], arity=one, inverse}).
