
-record(item, {uri, label, types=["type/thing"], properties=[]}).
-record(type, {uri, label, types=["type/type"], properties=[], parents=["type/thing"], legal_properties=[]}).
-record(property, {uri, label, types=["type/property"], properties=[], ranges=["type/thing"], arity=one, inverse}).
