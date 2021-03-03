import Elt._
import Exp._
import Type._

object Maybe {

  sealed trait Maybe  [a]
  case class   Just   [a](x: a) extends Maybe[a]
  case class   Nothing[a]()     extends Maybe[a]

  object Nothing_ {
    def apply[T,R](implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = nothing
    def unapply[T,R](x: Exp[Maybe[T],MRep[R]])(implicit elt: Elt[T,R]): Boolean = matchNothing(x)
  }

  object Just_ {
    def apply[T,R](x: Exp[T,R])(implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = just(x)
    def unapply[T,R](x: Exp[Maybe[T],MRep[R]])(implicit elt: Elt[T,R]): Option[Exp[T,R]] = matchJust(x)
  }

  // trait EltMaybe[T,R] extends Elt[Maybe[T],R] {
  //   def nothing: Exp[Maybe[T]]
  //   def just(x: Exp[T]): Exp[Maybe[T]]
  // }

  type MRep[R] = (Byte,(Unit,R))

  implicit def eltMaybe[T,R](implicit elt: Elt[T,R]): Elt[Maybe[T],MRep[R]]
    = new Elt[Maybe[T],MRep[R]] {
    // type Repr = (Byte,(Unit,elt.Repr))
    // type Prod = (Exp[Byte],(Exp[Unit],Exp[T]))
    def witness = TypeRtag(TypeRpair(TypeRunit(),elt.witness))

    def tags = List(TagRtag(0, TagRpair(TagRunit(),TagRundef()))) ++
      (for (t <- elt.tags) yield TagRtag(1, TagRpair(TagRunit(),t)))

    // def nothing: Exp[Maybe[T]] = {
    //   val ix: PreExp[Byte] = Constant(0)
    //   Exp(Tuple((Exp(ix), (Exp(UExp()),Exp(Undef()(elt)))))(this))
    // }

    // def just(x: Exp[T]): Exp[Maybe[T]] = {
    //   val ix: PreExp[Byte] = Constant(1)
    //   Exp(Tuple((Exp(ix), (Exp(UExp()),x)))(this))
    // }
  }

  // def nothing[T,R](implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = {
  //   val ix: PreExp[Byte] = Const(0)
  //   Exp(Tuple(Pair(ix, Pair(UExp(),Rep(Exp(Undef()(elt)))))))
  // }

  // def just[T,R](x: Exp[T,R])(implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = {
  //   val ix: PreExp[Byte] = Const(1)
  //   Exp(Tuple(Pair(ix, Pair(UExp(),Rep(x)))))
  // }

  def nothing[T,R](implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = {
    val ix: PreExp[Byte] = Const(0: Byte)
    Exp(Pair(ix, Pair(UExp(),Undef()(elt))))
  }

  def just[T,R](x: Exp[T,R])(implicit elt: Elt[T,R]): Exp[Maybe[T],MRep[R]] = {
    val ix: PreExp[Byte] = Const(1: Byte)
    Exp(Pair(ix, Pair(UExp(),x.e)))
  }

  def matchNothing[T,R](exp: Exp[Maybe[T],MRep[R]])(implicit elt: Elt[T,R]): Boolean = {
    exp match {
      case Exp( Match( TagRtag(0, TagRpair(TagRunit(),TagRundef())) , _) ) => true
      case Exp( Match (_,_) )                                    => false
      case _                                                     => sys.error("Expected match")
    }
  }

  def matchJust[T,R](x: Exp[Maybe[T],MRep[R]])(implicit elt: Elt[T,R]): Option[Exp[T,R]] = {
    x match {
      case Exp( Match( TagRtag(1, TagRpair(TagRunit(),t)) , e) ) => Some(Exp(Match(t,PrjR(PrjR(e)))))
      case Exp( Match(_, _) )                                    => None
      case _                                                     => sys.error("Expected match")
    }
  }

  // def matchJust[T,R](x: Exp[Maybe[T],MRep[R]])(implicit elt: Elt[T,R]): Option[Exp[T,R]] = {
  //   x match {
  //     case Exp(Tuple(Pair(ix,Pair(_,Rep(exp))))) => returnMaybe(exp.e,exp.elt.asInstanceOf[TypeTag[Any]])
  //     case _                                => None
  //   }
  // }

  // def returnMaybe[A,T:TypeTag,R](x: PreExp[A], t: TypeTag[A])(implicit elt: Elt[T,R]): Option[Exp[T,R]] = {
  //   t.tpe match {
  //     case m if m =:= typeOf[PreExp[T]] => returnJust(x, elt)
  //     case _                    => None
  //   }
  // }

  // def returnJust[A,T:TypeTag,R](x: PreExp[A], elt: Elt[T,R])(implicit ev: PreExp[A] =:= PreExp[T]): Option[Exp[T,R]] = {
  //   Some(Exp(ev(x))(elt))
  // }

  // def test[A](x: A)(implicit ev: A =:= Int): Int = 1 + x

}
