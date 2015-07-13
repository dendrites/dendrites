#!/usr/bin/env node
var dendrites = require('./dendrites.js');
dendrites('greet.js').invoke('greet', function (g) {
  console.log('He said ' + g);
});

