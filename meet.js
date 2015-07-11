#!/usr/bin/env node

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

write(args[1], 'ready').then(function () {
  return read(args[0]);
}).then(function (line) {
  read(args[0]).then(function (line) {
    console.log('got ' + line);
  });
  write(args[1], 'greet.sh bump');
});

