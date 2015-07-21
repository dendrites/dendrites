# dendrites

Dendrites allows for inter-language communication over pipes by providing each
of the processes it runs with a read- and a write-socket. This communication
layer will then be wrapped by client libraries, allowing the programmer to
cross between languages painlessly.

## Usage

Run your dendrites processes using the executable provided. The processes can
access the read and write pipe directly, but it is suggested that the client
libraries, once finished, be used.

```sh
$ runghc dendrites.hs meet.sh greet.sh
He said hello
```

