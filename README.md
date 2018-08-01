# inheritator2

`inheritator2` is a _draft_ interpreter for the Grace programming
language. The aim of the project (and we're nowhere near there yet) is
to design an AST-walking interpreter that is as straightforward as we
can make it, and then build a type checker on top of that.  The aim is
to produce a relatively readble and testable implementation of the
operational semantics and type system of the whole language.

## AST

To avoid re-writing a parser, inheritator2 is currently parasitc on
Kernan's parser. You need to install the "KernanCompiler.cs" module 
in the "modules/platform" directory to expose the compiler.
(You currently need to run on a hacked version of Kernan
to expose more of the parse tree via the execution tree:
this will change when we switch to using the parse tree)

Kernan's execution tree is transformed into a more abstract "common" AST
--- ideally this AST would be common across different Grace
implementations (ha!) for implementing type checkers etc. To port
inheritator2 to another Grace system, either that system should export
the common AST; we write a wedge that converts the native AST to the
common AST; or we write or get a Grace parser that produces the common
AST directly.

The current design of the common AST is preliminary.
More than that: it's interface is ugly and horrible and needs to be fixed. 

## Modules

- main.grace - iterates through command line arguments and evals each file

- loader.grace - loads modules, caches loaded modules

- kernan-translator.grace - translates from Kernan AST to common AST

- common-ast.grace - common AST defined as a class family

- evaluator.grace - extends common AST via family inheritance to provide
  the key `eval` method that embodies the interpreter.

- object-model.grace - objects, contexts (aka "Object Memory")

- attributes.grace - attributes (i.e. object fields, etc..)
     anything that can be found via a lookup, pretty much)
     - submodule of object-model

- primitives.grace - primitive objects (primitivesFamily)
     - submodule of object-model

- utility.grace - utility definitions: Constants, helper methods
     - import then use `exports` 

- errors.grace - errors, exceptions, assertions etc.
     - import then use `exports` 

- combinator-collections.grace - a hacked port of an old version of the
  collections library to Kernan. Includes a hack for a "post-lineup"
  collection interface, so e.g. `list` creates an empty list,
  `list(_)`, `list(_,_)` create one and two element lists etc.
     - import then use `abbreviations`

## Other files

- run-test.sh - runs end-to-end single file test

- tests - random collection of tests not curently working

- oldtests - deprecated tests

- misc - at some point move msic examples out of tests into here




