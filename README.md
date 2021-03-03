embedded-pattern-matching
=========================

Implementation to accompany the paper _Embedded Pattern Matching_ by ~~redacted~~.

With this technique we can use pattern matching in the _host_ language to
introduce case expressions in the _embedded_ language, in much the same way the
user would do in the host language. This enables a natural and user-friendly
embedding of user-defined algebraic data types into the embedded language. For
example:

```haskell
length :: Elt a => Exp (List a) -> Exp Int
length xs =
  let -- the core of the technique demonstrated here:
      body = match \case
        Nil_        -> int 0
        Cons_ _ xs' -> int 1 `Add` App (Var l) xs'

      -- because we demonstrate the technique using a first order embedding:
      l = Idx "length"
      v = Idx "xs"
  in
  Let l (Lam v (body (Var v))) (App (Var l) xs)
```

This repository contains three versions of the idea:

## haskell-repr

This implementation uses an embedded language where the AST witnesses terms
based on the _representation_ type of those terms. This is because it follows
the style used by the embedded language from the case study (Accelerate).

Supports sum and product data types, and includes all the GHC.Generics and
TemplateHaskell automation discussed in the paper.

See `haskell-repr/README.md` for more information.


## haskell-surface

This implementation in Haskell uses an embedded language where the AST witnesses
terms based on the _surface_ type of those terms (that is, the user's view of a
data type).

Supports sum, product, and recursive data types.

We have not yet copied over all of the GHC.Generics and TemplateHaskell
automation from the `haskell-repr` implementation, but that is a straightforward
process.

See `haskell-surface/README.md` for more information.


## scala-surface

This implementation in Scala is similar to the formulation in `haskell-surface`,
with some straightforward changes as described in the paper to account for
differences between Scala and Haskell.

Supports sum, product, and recursive data types.

See `scala-surface/README.md` for more information.

