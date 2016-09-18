package circular.generic.util

import shapeless.{::, DepFn1, HList, HNil}
import shapeless.ops.hlist.Tupler
import shapeless.ops.product.ToHList

/**
 * View an HList:
 * - empty     <=> Unit
 * - A :: HNil <=> A
 * - otherwise <=> Tupled form
 */
trait View[L <: HList] {
  type Out

  def view(l: L): Out
  def unview(o: Out): L
}

object View extends LowPriorityView {
  type Aux[L <: HList, Out0] = View[L] { type Out = Out0 }

  implicit val hnilView: View.Aux[HNil, Unit] = new View[HNil] {
    type Out = Unit
    def view(t: HNil): Unit = ()
    def unview(o: Unit): HNil = HNil
  }

  implicit def hsingleView[A]: View.Aux[A :: HNil, A] = new View[A :: HNil] {
    type Out = A
    def view(t: A :: HNil): A = t.head
    def unview(a: A): A :: HNil = a :: HNil
  }
}

private[util] sealed abstract class LowPriorityView {
  implicit def hlist[L <: HList, P <: Product](implicit L: Tupler.Aux[L, P], P: ToHList.Aux[P, L]): View.Aux[L, P] =
    new View[L] {
      type Out = P
      def view(t: L): P = L(t)
      def unview(o: P): L = P(o)
    }
}
