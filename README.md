# Ocaml-redis

Ocaml bindings for [Redis](http://redis.io/)

## Dependencies

* [Batteries](https://github.com/ocaml-batteries-team/batteries-included.git)
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
