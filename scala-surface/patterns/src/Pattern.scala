import Exp._
import Elt._

object Pattern {

  trait IsPattern[s, r] {
    def construct(r: r): Exp[s,r]
    def destruct (s: Exp[s,r]): r
  }

  def caseof[A,Z,M,N]
      (f: Exp[A,Z] => Exp[M,N])(implicit m: Elt[M,N]):
          Exp[A,Z] => Exp[M,N]
  = (z) => {
    Exp(Case(z.e,
      for (fa <- z.elt.tags)
        yield (fa, f( Exp(Match(fa,z.e))(z.elt) ).e)
    ))
  }

  def caseof[A,Z,B,Y,M,N]
      (f: (Exp[A,Z], Exp[B,Y]) => Exp[M,N])(implicit m: Elt[M,N]):
          (Exp[A,Z], Exp[B,Y]) => Exp[M,N]
  = (z,y) => {
    Exp(Case(z.e,
      for (fa <- z.elt.tags)
        yield (fa, caseof(f( Exp(Match(fa,z.e))(z.elt), _))(m)(y).e)
    ))
  }

  def caseof[A,Z,B,Y,C,X,M,N]
      (f: (Exp[A,Z], Exp[B,Y], Exp[C,X]) => Exp[M,N])(implicit m: Elt[M,N]):
          (Exp[A,Z], Exp[B,Y], Exp[C,X]) => Exp[M,N]
  = (z,y,x) => {
    Exp(Case(z.e,
      for (fa <- z.elt.tags)
        yield (fa, caseof(f(Exp(Match(fa,z.e))(z.elt),_,_))(m)(y,x).e)
    ))
  }

}
