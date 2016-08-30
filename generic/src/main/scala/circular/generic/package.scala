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
import shapeless.ops.hlist.Tupler
import shapeless.ops.product.ToHList

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
    def derive[C <: Coproduct, L <: HList, P <: Product](implicit
      TGeneric: Generic.Aux[T, C], // T <=> Coproduct
      DSelect : Selector[C, D],    // Coproduct => D
      DGeneric: Generic.Aux[D, L], // D <=> HList
      LTupler : Tupler.Aux[L, P],  // HList => TupleN

      PHList  : ToHList.Aux[P, L], // TupleN => HList
      DInject : Inject[C, D]       // D => Coproduct
    ): PIso[P, T] = {
      def to(p: P): Option[T] = Some(TGeneric.from(DInject(DGeneric.from(PHList(p)))))
      def from(t: T): Option[P] = DSelect(TGeneric.to(t)).map(d => LTupler(DGeneric.to(d)))
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
    def derive[L <: HList, P <: Product](implicit
      AGeneric: Generic.Aux[A, L], // A <=> HList
      LTupler : Tupler.Aux[L, P],  // HList => TupleN
      PHList  : ToHList.Aux[P, L]  // TupleN => HList
    ): PIso[P, A] = {
      def to(p: P): Option[A] = Some(AGeneric.from(PHList(p)))
      def from(a: A): Option[P] = Some(LTupler(AGeneric.to(a)))
      PIso(to, from)
    }
  }
}
