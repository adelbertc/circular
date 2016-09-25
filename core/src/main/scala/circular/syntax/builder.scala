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
package syntax

import cats.{Cartesian, SemigroupK}
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{Init, Last, Prepend}

trait BuilderSyntax {
  implicit def circularSyntaxBuilder[F[_], A](fa: F[A]): BuilderOps[F, A] =
    new BuilderOps(fa)

  implicit def circularSyntaxBuilderHList[F[_], L <: HList](fl: F[L]): BuilderOpsHList[F, L] =
    new BuilderOpsHList(fl)
}

final class BuilderOps[F[_], A](val fa: F[A]) extends AnyVal {
  def <+>(fb: F[A])(implicit F: SemigroupK[F]): F[A] = F.combineK(fa, fb)

  def <*>[B](fb: F[B])(implicit F0: Cartesian[F], F1: PInvariant[F]): F[A :: B :: HNil] = {
    val f = PIso[(A, B), A :: B :: HNil](p => Some(p._1 :: p._2 :: HNil), l => Some((l(0), l(1))))
    F1.pimap(F0.product(fa, fb))(f)
  }
}

final class BuilderOpsHList[F[_], L <: HList](val fl: F[L]) extends AnyVal {
  def <*>[B, O <: HList](fb: F[B])(implicit
    F0: Cartesian[F],
    F1: PInvariant[F],
    P : Prepend.Aux[L, B :: HNil, O],
    I : Init.Aux[O, L],
    L : Last.Aux[O, B]
  ): F[O] = {
    val f = PIso[(L, B), O](p => Some(P(p._1, p._2 :: HNil)), l => Some((I(l), L(l))))
    F1.pimap(F0.product(fl, fb))(f)
  }
}
