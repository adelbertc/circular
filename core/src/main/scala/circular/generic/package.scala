/*
 * Copyright 2016 Adelbert Chang
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package circular

import shapeless.{Coproduct, Generic, HList}
import shapeless.ops.coproduct.{Inject, Selector}

import circular.generic.util.View

package object generic {
  /**
   * Derive a [[PIso]] for a sum type (sealed family of case classes). The resulting
   * [[PIso]] will be between the tupled constructor of `D` and `T`.
   *
   * @tparam T Type constructor - the type of the trait or abstract class marked 'sealed'
   * @tparam D Data constructor - the type of the case class below `T`
   */
  def sum[T, D]: SumPartiallyApplied[T, D] = new SumPartiallyApplied[T, D]()

  /**
   * Derive a [[PIso]] for a product type (a case class). The resulting [[PIso]] will
   * be between the tupled constructor of `A` and `A`.
   *
   * @tparam A The case class
   */
  def product[A]: ProductPartiallyApplied[A] = new ProductPartiallyApplied[A]()
}

package generic {
  final class SumPartiallyApplied[T, D] {
    /**
     * Do the derivation.
     *
     * The type parameters and implicit arguments should be inferred - if they are not
     * please make sure your call to `derive` is correct.
     */
    def derive[C <: Coproduct, L <: HList, O](implicit
      T : Generic.Aux[T, C], // T <=> Coproduct
      C0: Selector[C, D],    // Coproduct => D
      D : Generic.Aux[D, L], // D <=> HList
      L : View.Aux[L, O],    // HList => View
      C1: Inject[C, D]       // D => Coproduct
    ): PIso[O, T] = {
      def to(o: O): Option[T] = Some(T.from(C1(D.from(L.unview(o)))))
      def from(t: T): Option[O] = C0(T.to(t)).map(d => L.view(D.to(d)))
      PIso(to, from)
    }
  }

  final class ProductPartiallyApplied[A] {
    /**
     * Do the derivation.
     *
     * The type parameters and implicit arguments should be inferred - if they are not
     * please make sure your call to `derive` is correct.
     */
    def derive[L <: HList, O](implicit
      A: Generic.Aux[A, L], // A <=> HList
      L: View.Aux[L, O]    // HList => View
    ): PIso[O, A] = {
      def to(o: O): Option[A] = Some(A.from(L.unview(o)))
      def from(a: A): Option[O] = Some(L.view(A.to(a)))
      PIso(to, from)
    }
  }
}
