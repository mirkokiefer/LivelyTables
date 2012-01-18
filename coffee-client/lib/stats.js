(function() {
  var stats, utils;

  utils = require('./utils');

  stats = {
    sum: function(array) {
      return array.reduce(function(previous, current) {
        return previous + current;
      });
    },
    mean: function(array) {
      return (stats.sum(array)) / array.length;
    },
    min: function(array) {
      return Math.min.apply(null, array);
    },
    absoluteFrequency: function(array, value) {
      var reduceFun;
      reduceFun = function(previous, current) {
        if (current === value) {
          return previous + 1;
        } else {
          return previous;
        }
      };
      return array.reduce(reduceFun, 0);
    },
    relativeFrequency: function(array, value) {
      return (stats.absoluteFrequency(array, value)) / array.length;
    },
    median: function(array) {
      var length, sortedArray;
      sortedArray = array.sort(utils.ascNum);
      if (((length = array.length) % 2) === 0) {
        return (sortedArray[length / 2 - 1] + sortedArray[length / 2]) / 2;
      } else {
        return sortedArray[Math.floor(length / 2)];
      }
    },
    variance: function(array) {
      var mean;
      mean = stats.mean(array);
      return 1 / array.length * utils.sum(array, function(each) {
        return Math.pow(each - mean, 2);
      });
    },
    standardDeviation: function(array) {
      return Math.sqrt(stats.variance(array));
    }
  };

  module.exports = stats;

}).call(this);
