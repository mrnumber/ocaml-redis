# Ocaml-redis

Ocaml bindings for [Redis](http://redis.io/). API documentation can be found [here](http://0xffea.github.io/ocaml-redis/).

Changelog can be found in [CHANGES.md](/CHANGES.md) file.


## Dependencies

* [Lwt](http://ocsigen.org/lwt/install) (optional)

## Quick start

### With OPAM

```
opam install redis
```

It you want Lwt support, but doesn't have Lwt installed:

```
opam install lwt redis
```

### Without OPAM

```
./configure --enable-lwt
make
make install
```
