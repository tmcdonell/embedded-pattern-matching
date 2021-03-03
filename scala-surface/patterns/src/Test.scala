import Maybe._
import Pattern._
import Exp._
import Elt._

object Test {

  def maybeAdd[M](x: Exp[Maybe[Int],MRep[Int]], i: Exp[Int,Int]): Exp[Int,Int] = {
    def go(e: Exp[Maybe[Int],MRep[Int]]) = e match {
      case Just_(n) => plus(n,i)
      case Nothing_() => i
    }

    caseof(go _)(eltInt)(x)
  }

  def maybeMaybeAdd[M](x: Exp[Maybe[Maybe[Int]],MRep[MRep[Int]]], i: Exp[Int,Int]): Exp[Int,Int] = {
    def go(e: Exp[Maybe[Maybe[Int]],MRep[MRep[Int]]]) = e match {
      case Just_(Just_(n)) => plus(n,i)
      case Nothing_() => i
      case _          => Exp(Const(500))
    }

    caseof(go _)(eltInt)(x)
  }

  def main(args: Array[String]): Unit = {
    println(maybeMaybeAdd(Just_(Just_(Exp(Const(1)))),(Exp(Const(5)))))
  }
}
