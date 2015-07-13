# dendrites

Dendrites allows for inter-language communication over pipes by providing each
of the processes it runs with a read- and a write-socket. This communication
layer is then wrapped by client libraries, allowing the programmer to cross
between languages painlessly.

## Usage

Run your dendrites processes using the executable provided. The processes can
access the read and write pipe directly, but it is suggested that the client
libraries be used, as in the following.

```javascript
// meet.js
var dendrites = require('./dendrites.js');
dendrites('greet.js').invoke('greet', function (g) {
  console.log('He said ' + g);
});
```

```javascript
// greet.js
require('./dendrites.js').provide('greet', function (fn) {
  fn('hello');
});
```

```sh
$ dendrites meet.js greet.js
He said hello
```

