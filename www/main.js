

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
    $('#types').text(item.types.join('<br>'));
    for(var property in item.properties) {
      $('#properties').append(property + ': ' + item.properties[property] + '<br>');
    }
  });
  testPut("mike", "employee", sampleItem());
  testPut("john", "employee", badItem());
  testPut("manager1", "type", sampleType());
  testPut("manager2", "type", badType());
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
    "types":["employee"],
    "properties": {
      "age":40,
      "salary":2000,
      "boss":"jim"
    }
  };
  return item;
};

var badItem = function() {
  var item = {
    "label":"John",
    "types":["employee"],
    "properties": {
      "age":"40"
    }
  };
  return item;
}

var sampleType = function() {
  var type = {
    "label":"Manager",
    "types":["type"],
    "parents":["employee"],
    "properties":[],
    "legal_properties":["manages"]
  };
  return type;
}

var badType = function() {
  var type = {
    "types":["type"],
    "parents":["employee"],
    "jklproprties":[]
  };
  return type;
}