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
- Higher-order units have been implemented. (But merging of two unit imports are not implemented yet.)
- First-class units are not implemented yet.
- Elaboration translation (and thus the dynamic semantics) still remains to be implemented.

## Additions to the original system in the paper

- Pattern matching: `match exp => {pattern => exp | ...}`
- Labeled sums: `LT () : [LT : unit, EQ : unit, GT : unit]`
- Units (empty tuples): `()` of type `unit`
- Tuples: `(3, true, ())` of type `int * bool * unit`
- Datatypes: `{data t = int}` of signature `{data t : int}`
  - In this case, `t_in` and `t_out` are generated as coercions between `t` and `int`.
  - Recursive datatypes: `link X = {type : Type} with new [unit {data t = ext X.t}]`

## Grammar

```
ident ::= upper_ident
        | lower_ident

upper_ident ::= ['A'..'Z'] ['A'..'Z' | 'a'..'z' | '0'..'9']*

lower_ident ::= ['a'..'z'] ['A'..'Z' | 'a'..'z' | '0'..'9']*

number ::= ['0'..'9']+
```

Note that `"->"` is right-associative.

```
kind ::= "Type"
       | kind "->" kind


type ::= type_tuple
       | "fun" "(" tvar ":" kind ")" "->" type
       | type "->" type

type_tuple ::= type_app
             | type_app "*" type_tuple

type_app ::= type_atom
           | "ext" module_proj
           | type_app type_atom

type_atom ::= "(" type ")"
            | tvar
            | "bool"
            | "int"
            | "unit"
            | "[" sum "]"

sum ::= con ":" type
      | con ":" type "," sum

con ::= upper_ident

tvar ::= "'" ident


exp ::= exp_app
      | "fun" param+ "->" exp
      | con exp_atom ":" type

exp_app ::= exp_atom
          | "ext" module_proj
          | exp_app exp_atom
          | "match" exp "=>" "{" "|"? branch ("|" branch)* "}"

exp_atom ::= "true"
           | "false"
           | number
           | lower_ident
           | "(" ")"
           | "(" exp_tuple ")"

exp_tuple ::= exp
            | exp "," exp_tuple

param ::= "(" pat ":" type ")"

pat ::= pat_atom
      | con pat_atom

pat_atom ::= lower_ident
           | "_"
           | "(" ")"
           | "(" pat_tuple ")"
           | "true"
           | "false"

pat_tuple ::= pat
            | pat "," pat_tuple

branch ::= pat "=>" exp


module ::= module_proj
         | "new" module_proj
         | "link" (modvar "=")? module "with" module
         | "link" (modvar "=")? module "seals" module

module_proj ::= module_atom
              | module_proj "." label

module_atom ::= "(" module ")"
              | modvar
              | "{" "}"
              | "[" "val" exp "]"
              | "[" "val" ":" type "]"
              | "[" "type" type "]"
              | "[" "type" ":" kind "]"
              | "[" "unit" module "]"
              | "[" "unit" ":" usig "]"
              | "{" dec "}"

dec ::= "module" label "=" module
      | "type" label "=" type
      | "type" label ":" kind
      | "val" label "=" exp
      | "val" label ":" type
      | "unit" label "=" module
      | "unit" label ":" usig
      | "data" label ":" type
      | "data" label "=" type

usig ::= module_proj "import" "(" path ("," path)* ")"
       | module_proj "export" "(" path ("," path)* ")"

path ::= label ("." label)*

modvar ::= upper_ident
         | lower_ident

label ::= upper_ident
        | lower_ident
```

## References

### Mixin’ up the ML module system

Andreas Rossberg and Derek Dreyer.
ACM Transactions on Programming Languages and Systems, 35(1), 2013.
https://doi.org/10.1145/2450136.2450137
