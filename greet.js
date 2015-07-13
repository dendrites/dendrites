#!/usr/bin/env node
require('./dendrites.js').provide('greet', function (fn) {
  fn('hello');
});

