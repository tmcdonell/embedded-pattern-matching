In the Scala version, because implicits are non-unique, we have to witness the `Exp` constructor on the
representation type in addition to the surface type. For example, the projection in `matchJust` requires
a certain structure to the representation (which the implementation of course uses), but the compiler
can't prove that it will be the only implementation that will exist - or that it will be using this implementation
if there's multiple ones.

To make this a bit less verbose, we can define a type synonym to talk about the representation type, but
it has to be just a simple type synonym - not a type family or associated type. This is because, even though
types can be defined on Scala implicit instances (a form of associated types), implicits are passed _after_
the regular arguments, and those arguments can't be referenced in earlier argument lists. For example, the
following doesn't work for that reason:

```
trait Elt[A] {
  type Repr
}

//                          | cannot reference `elt` here
case class Exp[A](x: PreExp[elt.Repr])(implicit elt: Elt[A])
```

I suspect that allowing these back-references wouldn't be enough to write the implementation word-for-word
like Haskell, since we still have the problem of wanting to project onto an unknown representation later on.
In a Haskell where we allowed multiple typeclass implementations, we might try to add an equality contraint
based on the instance stored in the GADT (i.e. Elt a => Const :: a -> PreExp a), but even then it seems like
it would require a runtime check on which instance is in use, so this equality constraint wouldn't work
without something like Haskell's `Typeable` reflection. Scala has a way of talking about equality constraints
which I haven't had much success with (`=:=`), and I haven't found an equivalent to `Just Refl`, since `Refl`
would equivalently be a constructor of `=:=` in Haskell, but in Scala this type is abstract.

It's simpler (not sure if required) to use the `Exp`/`PreExp` combination here because it allows us to
witness the `PreExp` type on only the representation. We could potentially do away with the wrapper and
just put both the type variables on the `PreExp` type, but then we have weird situations where we have
to have a pair in the representation and surface types of the `Pair` constructor - which is of course
correct but then we'd need an additional constructor (e.g. `Tuple`) to "cast" that surface type to another
surface type that's represented by that pair structure, and that `Tuple` constructor would have less use
compared to the `Exp` constructor, because the `Exp` constructor also stores the `Elt` implicit for the
type, which means functions don't have to include that in their contexts.

Our `caseof` (`match` is a keyword in Scala) function accepts uncurried functions, and just has multiple
overloaded implementations for multiple lengths of argument lists. Scala does have a way to, curry and
uncurry any function, but we require the implicit `Elt` for the output type to be in context, which would
get in the way of the recursion. Additionally, this implicit also needs to be passed in the use site:

```
//           | this implicit can't be skipped over before applying the returned function
caseof(go _)(eltInt)(x)
```
