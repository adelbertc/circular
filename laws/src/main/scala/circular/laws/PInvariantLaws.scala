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

import cats.laws.{IsEq, IsEqArrow}

trait PInvariantLaws[F[_]] {
  implicit def F: PInvariant[F]

  def pInvariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.pimap(fa)(PIso.id[A]) <-> fa

  def pInvariantComposition[A, B, C](fa: F[A], f: PIso[A, B], g: PIso[B, C]): IsEq[F[C]] =
    F.pimap(F.pimap(fa)(f))(g) <-> F.pimap(fa)(g.compose(f))
}

object PInvariantLaws {
  def apply[F[_]](implicit F0: PInvariant[F]): PInvariantLaws[F] =
    new PInvariantLaws[F] { def F: PInvariant[F] = F0 }
}
