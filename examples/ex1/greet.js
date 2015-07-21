#!/usr/bin/env node
require('../../lib/js/dendrites.js').provide('greet', function (fn) {
  fn('hello');
});

