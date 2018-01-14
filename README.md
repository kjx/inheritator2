# inheritator2

`inheritator2` is a _draft_ interpreter for the Grace programming
language. The aim of the project (and we're nowhere near there yet) is
to design an AST-walking interpreter that is as straightforward as we
can make it.  The aim is to produce a relatively readble and testable
description of the operational semantics of the whole language.

## AST

To avoid re-writing a parser, inheritator2 is currently parasitc on
Kernan's parser. Kernan's execution tree is transformed into more
abstract AST --- ideally this AST would be common across different
Grace implementations for implementing type checkers etc. To port
inheritator2 to another Grace system, either that system should export
the common AST; we write a wedge that converts the native AST to the
common AST; or we write or get a Grace parser that produces the common
AST directly. 

Of course, the current design of the common AST is preliminary.
More than that: it's ugly and horrible and needs to be fixed. 

## Design

`inheritator2` is currently implemented as a dialect that runs on
Kernan. The dialect `jkernandialect` uses the checker interface to get
the AST then inteprets that AST. Once that's done, Kernan will execute
the input file "normally".  So, every program currently runs twice. 

- jast.grace - common AST a class family

- jeval.grace - extends common AST via family inheritance to provide
  the key `eval` method that embodies the interpreter.

- jruntime.grace - runtime model (aka "Object Memory")

- jcommon.grace - common definitions: Constants, helper methods
     - import then use `exports` 

- jerrors.grace - errors, exceptions, assertions etc.
     - import then use `exports` 

- jkernandialect.grace - wedge that runs as a Kernan dialect, builds
  the common AST and calls the interpreter.

- combinator-collections.grace - a port of an old version of the
  collections library to Kernan. Includes a hack for a "post-lineup"
  collection interface, so e.g. `list` creates an empty list,
  `list(_)`, `list(_,_)` create one and two element lists etc.
     - import then use `abbreviations`

## Other files

- jtest.grace main single pass test, should test each main feature,
    should always run to completion
- EXP-jtest.txt expected output for jtest.grace




- hello.grace - vanilla hello world
- jhello.grace - hello world that runs in inheritator2
- jGIOP.grace - grace in one page that builds AST in inh2, but doesn't run
      because there aren't enough operations on the primitive types 
- io.grace - fake IO module needed to run GIOP.grace
- jcollections.grace - colletion that parses in inh2, also doesn't run





