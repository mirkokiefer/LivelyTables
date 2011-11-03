

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
  testPut("mike", sampleItem());
  testPut("john", badItem());
});

var testPut = function(id, item) {
  $.ajax({
        type: 'PUT',
        async: true,
        data: JSON.stringify(item),
        contentType: 'application/json',
        url: "/employee/" + id,
        success: function(jsonData) {
          console.log(jsonData);
        }
      });
};

var sampleItem = function() {
  var item = {
    "uri":"mike",
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
    "uri":"john",
    "label":"John",
    "types":["employee"],
    "properties": {
      "age":"40"
    }
  };
  return item;
}