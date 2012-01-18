(function() {
  var request, write, writeItem;

  request = require('request');

  write = function(data, URI, cb) {
    var options, response;
    options = {
      method: 'PUT',
      url: URI,
      body: JSON.stringify(data)
    };
    console.log(options);
    response = function(err, res, body) {
      return cb(err, JSON.parse(body));
    };
    return request(options, response);
  };

  writeItem = function(item, cb) {
    return write(item, item.uri, cb);
  };

  module.exports = {
    write: write,
    writeItem: writeItem
  };

}).call(this);
