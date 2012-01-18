(function() {
  var utils;

  utils = {
    ascNum: function(a, b) {
      return a - b;
    },
    sum: function(range, fun) {
      var i, j, sum, _len;
      sum = 0;
      for (j = 0, _len = range.length; j < _len; j++) {
        i = range[j];
        sum = sum + fun(i, j);
      }
      return sum;
    },
    sumEach: function(array, fun) {
      var reduceFun;
      reduceFun = function(previous, current) {
        return previous + fun(current);
      };
      return array.reduce(reduceFun, 0);
    }
  };

  module.exports = utils;

}).call(this);
