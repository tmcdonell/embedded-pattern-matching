object Type {

  sealed trait TypeR[a]
  case class TypeRunit        ()                         extends TypeR[Unit]
  case class TypeRscalar[a]   (s: ScalarType[a])         extends TypeR[a]
  case class TypeRtag   [a]   (t: TypeR[a])              extends TypeR[(Byte, a)]
  case class TypeRpair  [a, b](x: TypeR[a], y: TypeR[b]) extends TypeR[(a, b)]
  case class TypeRrec[T]      (t: TypeR[T])              extends TypeR[Rec[T]]

  sealed trait ScalarType[a]
  case class NumScalarType[a](n: NumType[a])       extends ScalarType[a]
  case class NonNumScalarType[a](n: NonNumType[a]) extends ScalarType[a]

  sealed trait NumType[a]
  case class IntegralNumType[a](n: IntegralType[a]) extends NumType[a]
  case class FloatingNumType[a](n: FloatingType[a]) extends NumType[a]

  sealed trait IntegralType[a]
  case class TypeInt ()  extends IntegralType[Int]
  case class TypeByte()  extends IntegralType[Byte]

  sealed trait FloatingType[a]
  case class TypeFloat() extends FloatingType[Float]

  sealed trait NonNumType[a]
  case class TypeChar() extends NonNumType[Char]
  case class TypeBool() extends NonNumType[Boolean]

  def undef[t](x: TypeR[t]): t = {
    x match {
      case TypeRunit()     => ()
      case TypeRscalar(s)  => 0.asInstanceOf[t]
      case TypeRtag(t)     => (0xff.toByte, undef(t))
      case TypeRpair(l, r) => (undef(l), undef(r))
      case TypeRrec(_)     => null
    }
  }

}
