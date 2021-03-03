import Elt._
import Type._

// object Exp {

//   case class Exp[a, r](r: PreExp[r])(implicit repr: EltRepr[a, r])

//   sealed trait PreExp[a]

//   case class Constant[a]  (s: ScalarType[a], x: a)      extends PreExp[a]
//   case class Pair    [a,b](x: PreExp[a], y: PreExp[b])  extends PreExp[(a,b)]
//   case class UExp         ()                            extends PreExp[Unit]
//   case class PrjL    [a,b](x: PreExp[(a,b)])            extends PreExp[a]
//   case class PrjR    [a,b](x: PreExp[(a,b)])            extends PreExp[b]

//   case class Undef   [a]  (x: TypeR[a])                 extends PreExp[a]
//   case class Match   [a]  (t: TagR[a], x: PreExp[a])    extends PreExp[a]
//   case class Case    [a,b](x: PreExp[a], js: List[(TagR[a], PreExp[b])]) extends PreExp[b]

//   case class Var     [a]  (i: Idx[a])                   extends PreExp[a]
//   case class Let     [a,b](i: Idx[a], x: PreExp[a], y: PreExp[b]) extends PreExp[b]

//   case class Idx[t](x: TypeR[t], t: String)

// }

object Exp {

  // sealed trait Exp[T]

  case class Exp[T,R](e: PreExp[R])(implicit val elt: Elt[T,R])

  def plus[R](x: Exp[Int,Int], y: Exp[Int,Int]) = Exp(Plus(x.e, y.e))

  sealed trait PreExp[+R]

  // case class Rep[T,R] (e: Exp[T,R])(implicit elt: Elt[T,R]) extends PreExp[R]
  // case class Surface[T,R] (e: PreExp[R])(implicit elt: Elt[T,R]) extends PreExp[T]

  case class Const   [T,R] (e: T)        (implicit elt: Elt[T,R]) extends PreExp[R]
  // case class Tuple   [T,R] (e: PreExp[R])(implicit elt: Elt[T,R]) extends PreExp[T]

  case class Pair    [A,B](x: PreExp[A], y: PreExp[B])  extends PreExp[(A,B)]
  case class UExp         ()                            extends PreExp[Unit]
  case class PrjL    [A,B](x: PreExp[(A,B)])            extends PreExp[A]
  case class PrjR    [A,B](x: PreExp[(A,B)])            extends PreExp[B]

  case class Undef   [T,R] ()(implicit elt: Elt[T,R])     extends PreExp[R]
  case class Match   [R] (t: TagR[R], x: PreExp[R]) extends PreExp[R]
  case class Case    [a,b](x: PreExp[a], js: List[(TagR[a], PreExp[b])]) extends PreExp[b]

  // case class Var     [a]  (i: Idx[a])                   extends PreExp[a]
  // case class Let     [a,b](i: Idx[a], x: PreExp[a], y: PreExp[b]) extends PreExp[b]

  // case class Idx[t](x: TypeR[t], t: String)

  case class Unroll[T,R](x: Exp[Rec[T],Rec[R]]) extends PreExp[R]
  case class Roll  [T,R](x: Exp[T,R])           extends PreExp[Rec[R]]

  case class Plus(x: PreExp[Int], y: PreExp[Int]) extends PreExp[Int]

}
