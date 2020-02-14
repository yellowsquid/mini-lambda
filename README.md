# mini-lambda

Minimal lambda language for the Compiler Construction course supervisions.

## Development

### Prerequisites

You need the following to be able to create the development environment.

- [dune]
- [make]
- [opam]

Most Linux distributions have packages available for these tools.

[dune]: https://dune.build
[make]: https://www.gnu.org/software/make/
[opam]: https://opam.ocaml.org/

### Hacking

Create the development environment.

``` shell
make dev-switch
```

Run tests.

``` shell
make test
```

Create static documentation.

``` shell
make doc
```

Create live documentation.

``` shell
make livedoc
```

## Installation

First build the executable.

``` shell
make
```

Then install.

``` shell
make install
```

## Running

Compile from `lambda` to `x86_64` assembly. You will need to link the [runtime] manually.

[runtime]: #Runtime

``` shell
lambda -o <output.S> <input.lambda>
```

Run the `Ocaml` interpreter.

``` shell
lambda -i0 <input.lambda>
```

Run a lower-level `Ocaml` interpreter.

``` shell
lambda -i6 <input.lambda>
```

### Runtime

Build the runtime.

``` shell
cc -c runtime/runtime.c -o runtime/runtime.o
```

Link the runtime.

``` shell
cc <input.S> runtime/runtime.o -o <output>
```
