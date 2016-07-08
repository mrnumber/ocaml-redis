# Changelog

## 0.3.0

Now package contains 3 modules: `Redis`, `Redis_lwt` and `Redis_sync`.

* `Redis` - `Client`/`Cache`/`Mutex` modules type signatures
* `Redis_sync` - synchronous implementation of client library
* `Redis_lwt` - Lwt-based implementation of client library

Commands implementations:

* A few improvements to the sorted set operations, thanks @domsj
* Add PFADD/PFCOUNT/PFMERGE
* Add HSCAN/HSTRLEN/HINCRBYFLOAT commands
* Add MIGRATE command
* Add PSETEX and OBJECT command
* Add PUNSUBSCRIBE/PSUBSCRIBE commands, thanks @j0sh.
* Add MSET/MSETNX/MGET commands
* Add ZSCORE, thanks @ipfix
* Fail explicitly when PING command was failed

Testing changes:

* Rework all test cases due IO module usage, thanks @rgrinberg
* Fix test exit code to return non-zero code on failure

Infrastructure changes:

* String.create -> Bytes.create to silence warning on recent OCaml versions
* Require OCaml version to be >= 4.01.0, thanks @hcarty
* Replace `Lwt_chan` use with `Lwt_io`
* Properly resolve string hostnames, e.g. localhost, google.com etc.., thanks @toots
