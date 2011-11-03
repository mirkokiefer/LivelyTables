

$(function() {
  var uriTokens = window.location.pathname.split('/');
  uriTokens.pop();
  console.log(uriTokens);
  var uri = uriTokens.join('/');
  console.log(uri);
  $.get(uri, function(result, error) {
    var item = JSON.parse(result);
    $('#label').text(item.label);
    $('#types').text(item.types.join('<br>'));
    for(var property in item.properties) {
      $('#properties').append(property + ': ' + item.properties[property] + '<br>');
    }
  });
});