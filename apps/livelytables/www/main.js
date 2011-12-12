

$(function() {
  var uriTokens = window.location.pathname.split('/');
  uriTokens.pop();
  console.log(uriTokens);
  var uri = uriTokens.join('/');
  console.log(uri);
  $.get(uri, function(result, error) {
    var item = JSON.parse(result);
    console.log(result);
    $('#label').text(item.label);
    delete item["label"];
    for(var property in item) {
      $('#properties').append(property + ': ' + item[property] + '<br>');
    }
  });
  testPut("mike", "employee", sampleItem());
  testPut("john", "employee", badItem());
  testPut("manager1", "type", sampleType());
  testPut("manager2", "type", badType());
  testPut("owns", "property", sampleProperty());
  testPut("person", "type", updateType());
  testPut("graham", "person", compositeItem());
});

var testPut = function(id, type, item) {
  $.ajax({
    type: 'PUT',
    async: true,
    data: JSON.stringify(item),
    contentType: 'application/json',
    url: "/" + type + "/" + id,
    success: function(jsonData) {
      console.log(jsonData);
    }
  });
};

var sampleItem = function() {
  var item = {
    "label":"Mike",
    "age":40,
    "salary":2000,
    "boss":"jim"
  };
  return item;
};

var badItem = function() {
  var item = {
    "label":"John",
    "age":"40"
  };
  return item;
}

var sampleType = function() {
  var type = {
    "label":"Manager",
    "parents":["employee"],
    "legal_properties":["manages"]
  };
  return type;
}

var badType = function() {
  var type = {
    "parents":["employee"],
    "jklproprties":[]
  };
  return type;
}

var sampleProperty = function() {
  var property = {
    "label":"Owns",
    "range":"item",
    "arity":"many",
    "optional": true
  };
  return property;
}

var updateType = function() {
  var type = {
    "legal_properties": ["age", "owns"]
  };
  return type;
}

var compositeItem = function() {
  var item = {
    "label": "Graham",
    "age": 30,
    "owns": [{"uri":"inspired", "label": "Inspired", "types": ["company"]}]
  };
  return item;
}