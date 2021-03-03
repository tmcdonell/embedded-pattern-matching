# About

This example implements the pattern synonym AST formation that the sibling Haskell version does.
We use `.apply` and `.unapply` with _extractor objects_ as the equivalent to Haskell's pattern
synonym feature. It supports the use of products, sums, and nesting as the sibling does, however
we stop at front-end AST formation for the purpose of demonstrating the application of this
technique using Scala features. Types and AST constructors for recursive types are demonstrated
but not fully implemented. Scala has generics libraries but the provided implementation uses
manually written instances.

# Usage

With `mill` (https://github.com/com-lihaoyi/mill) installed:

`mill patterns.run`

This will run the `Test` module's `main` method. An example is provided here, which shows the use
of a nested sum pattern match of type `Maybe[Maybe[Int]]`, and the final AST is printed to the
console.

# Layout

Differences in Scala compared to Haskell that were encountered during the implementation of this
version are discussed in `commentary.md`. Previous attempts are retained in comments.

The implementation itself attempts to mirror the Haskell project layout, so everything will be in
an equivalent module.

The example type is named `Maybe` for demonstration purposes, but in reality we would implement
this instance on Scala's existing `Option` type.
