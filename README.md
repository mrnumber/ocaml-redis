# Ocaml-redis

Ocaml bindings for [Redis](http://redis.io/)

## Dependencies

* [ExtLib](http://code.google.com/p/ocaml-extlib/)

(but see the [META](https://github.com/mrnumber/ocaml-redis/blob/master/src/META) to be sure)

## Quick start

1. make
2. make install

## Example

Synchronous and asynchronous clients created with the [seed](http://github/mrnumber/seed.git) library:

```
module Sync = Redis.Make(Util_lwt.Sync)
module Async = Redis.Make(Util_lwt.Async)
```
