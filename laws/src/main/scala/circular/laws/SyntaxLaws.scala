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
package laws

import cats.Eq
import cats.laws.{CartesianLaws, IsEq, IsEqArrow, MonoidKLaws}

trait SyntaxLaws[F[_]] extends CartesianLaws[F] with MonoidKLaws[F] with PInvariantLaws[F] {
  implicit def F: Syntax[F]

  def syntaxLeftIdentity[A, B: Eq](fa: F[A], b: B): IsEq[F[A]] = {
    val f = PIso.fromIso[(B, A), A](_._2, (b, _))
    F.pimap(F.product(F.pure(b), fa))(f) <-> fa
  }

  def syntaxRightIdentity[A, B: Eq](fa: F[A], b: B): IsEq[F[A]] = {
    val f = PIso.fromIso[(A, B), A](_._1, (_, b))
    F.pimap(F.product(fa, F.pure(b)))(f) <-> fa
  }

  def syntaxAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, (B, C))]] =
    F.product(fa, F.product(fb, fc)) <-> F.pimap(F.product(F.product(fa, fb), fc))(PIso.associate[A, B, C].invert)
}

object SyntaxLaws {
  def apply[F[_]](implicit F0: Syntax[F]): SyntaxLaws[F] =
    new SyntaxLaws[F] { def F: Syntax[F] = F0 }
}
