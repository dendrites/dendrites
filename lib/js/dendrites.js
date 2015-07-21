var fs = require('fs');
var Q = require('q');
var args = process.argv.slice(2);
var exec = require('child_process').exec;

var read = function (file) {
  var resp = Q.defer();
  exec('read line < ' + file + ' && echo $line', function (err, line) {
    resp.resolve(line);
  });
  return resp.promise;
};

var write = function (file, msg) {
  var resp = Q.defer();
  exec('echo "' + msg + '" > ' + file, function (err, line) {
    resp.resolve(null);
  });
  return resp.promise;
};

var channel = write(args[1], 'ready').then(function () {
  return read(args[0]);
}).then(function () {
  return {
    read: function () {
      return read(args[0]);
    },
    write: function (msg) {
      return write(args[1], msg);
    }
  };
});

var listen = function (channel) {
  channel.read().then(function (line) {
    var src = line.split(' ')[0];
    var msg = JSON.parse(new Buffer(line.split(' ')[1], 'base64').toString('ascii'));
    var args = msg.args
                  .map(function (x) {
                    if (x.match(/^\[[^\]]+\]$/)) {
                      return function () {
                        var name = 'm' + x.substr(1, x.length - 2);
                        dendrites(src).invoke.apply({}, [name].concat([].slice.call(arguments)));
                      };
                    } else {
                      return x;
                    }
                  });
    var method = msg.method;
    methods[method].apply({}, args);
    listen(channel);
  });
};
channel.then(listen);

var methods = {}, methodCount = 0;
var dendrites = function (other) {
  return {
    invoke: function (method) {
      var args = [].slice.call(arguments, 1)
                   .map(function (x) {
                     if (typeof x === 'function') {
                       methods['m' + methodCount] = function (src) {
                         return x.apply({}, [].slice.call(arguments));
                       };
                       return '[' + methodCount + ']';
                     } else {
                       return x;
                     }
                   });
      return channel.then(function (channel) {
        var msg = new Buffer(JSON.stringify({
          method: method,
          args: args
        })).toString('base64');
        return channel.write(other + ' ' + msg);
      });
    }
  };
};
dendrites.provide = function (name, fn) {
  methods[name] = fn;
};
module.exports = dendrites;

