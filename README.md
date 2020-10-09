embedded-pattern-matching
=========================

Implementation to accompany the paper _Embedded Pattern Matching_ by Trevor L.
McDonell, Joshua Meredith, and Gabriele Keller (in submission).

## Building

 * Download and install the [stack build tool](https://docs.haskellstack.org/en/stable/README/)

 * Run the commands in the project directory:
   - `stack setup`
   - `stack build`

 * Some example program expressions can be generated with the commands:
   - `stack repl src/Test.hs`
   - `test_exp`


## Structure

All the files are contained in the `src/` directory. The main files are:

  - `Exp.hs`: The expression language described in Section 4
  - `Type.hs`: Contains the data type `PrimType` which defines the set of primitive types for this example language
  - `Elt.hs`: The type class `Elt` from Section 4
  - `Pattern.hs`: The polymorphic pattern synonym `Pattern` and type class `IsPattern` developed in Section 5.1
  - `Tag.hs`: The `TraceR` structure for tagging algebraic data types from Section 5.2.3
  - `TH.hs`: Implementation of the `mkPattern` TemplateHaskell magic mentioned in Section 5.2.3 to automatically generate pattern synonyms for sum data types. In order to view the generated pattern synonyms together with their builder and matcher functions, add (or uncomment) the line `{-# OPTIONS_GHC -ddump-splices #-}` for example in the files `Bool.hs`, `Maybe.hs`, and `Either.hs`.
  - `Match.hs`: Implementation of the `match` function from Section 5.2.4

