import Type._
import scala.reflect.runtime.universe._

object Elt {

  sealed trait TagR[+A]
  case class TagRunit     ()                    extends TagR[Unit]
  case class TagRscalar[A]()                    extends TagR[A]
  case class TagRundef [A]()                    extends TagR[A]
  case class TagRtag   [A](n: Byte, t: TagR[A]) extends TagR[(Byte,A)]
  case class TagRpair  [A,B](a: TagR[A], b: TagR[B]) extends TagR[(A,B)]

  trait Elt[T,R] {
    def witness: TypeR[R]
    def tags: List[TagR[R]]
  }

  trait Eltr[T] {
    type Repr
    def witness: TypeR[Repr]
  }

  implicit def eltByte: Elt[Byte,Byte] = new Elt[Byte,Byte] {
    def witness = TypeRscalar(NumScalarType(IntegralNumType(TypeByte())))
    def tags = List(TagRscalar())
  }

  implicit def eltInt: Elt[Int,Int] = new Elt[Int,Int] {
    def witness = TypeRscalar(NumScalarType(IntegralNumType(TypeInt())))
    def tags = List(TagRscalar())
  }

  implicit def eltRec[T,R](implicit elt: Elt[T,R]): Elt[Rec[T],Rec[R]] = new Elt[Rec[T],Rec[R]] {
    def tags = List()
    def witness = TypeRrec(elt.witness)
  }

}
