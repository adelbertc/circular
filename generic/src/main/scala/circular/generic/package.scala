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
  def derive[T, D]: DerivePartiallyApplied[T, D] = new DerivePartiallyApplied[T, D]()
}

package generic {
  final class DerivePartiallyApplied[T, D] {
    def pIso[C <: Coproduct, L <: HList, P <: Product](implicit
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
}
