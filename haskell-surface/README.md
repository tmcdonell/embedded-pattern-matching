Haskell (surface type encoding)
===============================

This version has the AST witnessed by the _surface_ type of the terms.

## Building

 * Download and install the [stack build tool](https://docs.haskellstack.org/en/stable/README/)

 * Run the commands in the project directory:
   - `stack setup`
   - `stack build`

 * Some example program expressions can be generated with the commands:
   - `stack repl src/Test.hs`
   - `length t1`
   - `eval $ length t1`

 * See also the file `Lambda.hs` where we demonstrate an embedded function to
   translate embedded lambda calculus terms into embedded SKI combinator terms


## Structure

All the files are contained in the `src/` directory. The main files are:

  - `Exp.hs`: The expression language described in Section 4
  - `Type.hs`: Contains the data type `PrimType` which defines the set of primitive types for this example language
  - `Elt.hs`: The type class `Elt` from Section 4
  - `Pattern.hs`: The polymorphic pattern synonym `Pattern` and type class `IsPattern` developed in Section 5.1
  - `Trace.hs`: The `TraceR` structure for tagging algebraic data types from Section 5.3
  - `Match.hs`: Implementation of the `match` function from Section 5.5

