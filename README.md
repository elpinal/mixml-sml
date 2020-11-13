# mixml-sml

A MixML typechecker, written in Standard ML.

## What is MixML?

MixML [Rossberg and Dreyer 2013] is a language unifying ML modules and mixin modules.
It supports separate compilation of mutually-recursive modules
while keeping almost all important features of ML modules.
In MixML, typechecking is decidable, and thus making it a practical language.
While most existing ML module systems with recursive modules (such as Moscow ML and OCaml) suffer from
two typing difficulties (one refers to how to identify internal and external views of abstract types, and the other refers to decidability of type equivalence in the presence of equi-recursive types at higher kinds), MixML solves such problems in a satisfactory manner.

## Getting started

To install, you need a Standard ML compiler.
MLKit and MLton are supported.
You are also required to have [cmtool](https://www.cs.cmu.edu/~crary/cmtool/).

### Build with MLKit

I recommend using MLKit (rather than MLton) because MLKit builds faster.
With `mlkit` and `cmyacc` in your `$PATH`, run the following commands:

```bash
$ make mlkit
$ ./mixml-mlkit
```

### Build with MLton

Alternatively, you can use MLton to build.
With `mlton` and `cmyacc` in your `$PATH`, run the following commands:

```bash
$ make mlton
$ ./mixml-mlton
```

## Status

- Few bugs are found.
- Higher-order units and first-class units are not implemented yet.
- Elaboration translaton (and thus the dynamic semantics) still remains to be implemented.

## References

### Mixin’ up the ML module system

Andreas Rossberg and Derek Dreyer.
ACM Transactions on Programming Languages and Systems, 35(1), 2013.
https://doi.org/10.1145/2450136.2450137
